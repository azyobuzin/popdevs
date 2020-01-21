module internal PopDEVS.ProcessOriented.IntermediateTree

open System
open FSharp.Quotations
open LetRecUtils
open PgUtils

[<RequireQualifiedAccess>]
type Tree =
    | Expr of FsExpr * cont: Tree
    | Let of FsVar * cont: Tree
    | If of t: Tree * f: Tree * cont: Tree
    | While of cond: Tree option * body: Tree * cont: Tree
    | Bind of cont: Tree
    | Zero

module Tree =
    let (|SimpleExpr|_|) = function
        | Tree.Expr (expr, Tree.Zero) -> Some expr
        | _ -> None

    let rec lastIsBind = function
        | Tree.Expr (_, x) | Tree.Let (_, x)
        | Tree.If (_, _, x) | Tree.While (_, _, x) -> lastIsBind x
        | Tree.Bind Tree.Zero -> true
        | Tree.Bind x -> lastIsBind x
        | Tree.Zero -> false

    /// `Tree.Bind` が含まれない `Tree` ならば、 `FsExpr` を作成する
    let rec rebuildExpr tree =
        let rec rebuild tree stack =
            match tree with
            | Tree.Expr (expr, cont) -> rebuild cont (expr :: stack)
            | Tree.Let (var, cont) ->
                let expr = stackToExpr stack
                match rebuild cont [] with
                | Some body -> Some (continueWithLet var expr body)
                | None -> None
            | Tree.If (t, f, cont) ->
                match rebuildExpr t, rebuildExpr f with
                | Some t, Some f ->
                    let condExpr = stackToExpr stack
                    rebuild cont [FsExpr.IfThenElse(condExpr, t, f)]
                | _ -> None
            | Tree.While (cond, body, cont) ->
                let condExpr =
                    match cond with
                    | Some x -> rebuildExpr x
                    | None -> Some (FsExpr.Value(true))
                match condExpr, rebuildExpr body with
                | Some cond, Some body -> rebuild cont [FsExpr.WhileLoop(cond, body)]
                | _ -> None
            | Tree.Bind _ -> None
            | Tree.Zero -> Some (stackToExpr stack)
        and stackToExpr = function
            | [] -> unitExpr
            | [expr] -> expr
            | expr :: stack -> mkSeqExpr (stackToExpr stack) expr
        rebuild tree []
        
    let reduce tree =
        match rebuildExpr tree with
        | Some expr -> Tree.Expr (expr, Tree.Zero)
        | None -> tree

let private varIsUsed var =
    let rec f = function
        | ExprShape.ShapeVar x -> x = var
        | ExprShape.ShapeLambda (_, x) -> f x
        | ExprShape.ShapeCombination (_, exprs) -> List.exists f exprs
    f

module private BuilderPatterns =
    let (|CallBind|_|) builder = function
        | Patterns.Call (Some (Patterns.Value (receiver, _)), method, [computation; binder])
                when obj.Equals(receiver, builder) && method.Name = "Bind" ->
            match binder with
            | Patterns.Lambda (var, body) -> Some (computation, var, body)
            | _ -> failwith "The second argument of the Bind call is not a lambda."
        | _ -> None

    let (|CallCombine|_|) builder = function
        | Patterns.Call (Some (Patterns.Value (receiver, _)), method, [left; right])
                when obj.Equals(receiver, builder) && method.Name = "Combine" ->
            Some (left, right)
        | _ -> None

    let (|CallWhile|_|) builder = function
        | Patterns.Call (Some (Patterns.Value (receiver, _)), method, [guard; computation])
                when obj.Equals(receiver, builder) && method.Name = "While" ->
            match guard with
            | Patterns.Lambda (_, cond) -> Some (cond, computation)
            | _ -> failwith "The first argument of the While call is not a lambda."
        | _ -> None

    let (|CallZero|_|) builder = function
        | Patterns.Call (Some (Patterns.Value (receiver, _)), method, [])
                when obj.Equals(receiver, builder) && method.Name = "Zero" ->
            Some ()
        | _ -> None

    let (|CallDelay|_|) builder = function
        | Patterns.Call (Some (Patterns.Value (receiver, _)), method, [generator])
                when obj.Equals(receiver, builder) && method.Name = "Delay" ->
            match generator with
            | Patterns.Lambda (_, body) -> Some body
            | _ -> failwith "The argument of the Delay call is not a lambda."
        | _ -> None

let toTree (input: ProcessModelBuilderResult<'I>) =
    let builder = input.Builder

    let mutable tmpVarCount = 0
    let tmpVar (name, ty) =
        tmpVarCount <- tmpVarCount + 1
        FsVar(sprintf "%s%d" name tmpVarCount, ty)

    let rec continueWith cont tree =
        let rec f = function
            | Tree.Expr (x, Tree.Zero) ->
                match cont with
                | Tree.Expr (y, z) -> Tree.Expr (FsExpr.Sequential(x, y), z)
                | _ -> Tree.Expr (x, cont)
            | Tree.Expr (x, y) -> Tree.Expr (x, f y)
            | Tree.Let (x, y) -> Tree.Let (x, f y)
            | Tree.If (x, y, Tree.Zero) ->
                // if の結果を次のノードで使うなら let する
                let varOpt =
                    match cont with
                    | Tree.If _ -> Some (tmpVar ("if", typeof<bool>))
                    | Tree.Bind _ -> Some (tmpVar ("if", typeof<WaitCondition>))
                    | _ -> None
                match varOpt with
                | Some var ->
                    Tree.If (x, y,
                        Tree.Let (var,
                            Tree.Expr (FsExpr.Var(var),
                                cont)))
                | None -> Tree.If (x, y, cont)
            | Tree.If (x, y, z) -> Tree.If (x, y, f z)
            | Tree.While (x, y, z) -> Tree.While (x, y, f z)
            | Tree.Bind Tree.Zero ->
                // 結果を次のノードで使うなら let する
                let varOpt =
                    match cont with
                    | Tree.If _ -> Some (tmpVar ("bind", typeof<bool>))
                    | Tree.Bind _ -> Some (tmpVar ("bind", typeof<WaitCondition>))
                    | _ -> None
                match varOpt with
                | Some var ->
                    Tree.Bind (
                        Tree.Let (var,
                            Tree.Expr (FsExpr.Var(var),
                                cont)))
                | None -> Tree.Bind cont
            | Tree.Bind x -> Tree.Bind (f x)
            | Tree.Zero -> cont
        f tree

    let rec toTreeCore = function
        | Patterns.Let (var, expr, body) ->
            toTreeCore expr
            |> continueWith (letIfNeeded (var, body))
            |> Tree.reduce

        | Patterns.LetRecursive (bindings, body) ->
            let bindings = bindingsWithKind bindings

            let makeMutable (v: FsVar, e: FsExpr) (b: FsExpr) =
                if v.IsMutable then v, e, b
                else
                    let mutVar = FsVar(v.Name, v.Type, true)
                    let mutVarExpr = FsExpr.Var(mutVar)
                    let rec replaceVar = function
                        | ExprShape.ShapeVar x as e ->
                            if x = v then mutVarExpr else e
                        | ExprShape.ShapeLambda (x, e) ->
                            FsExpr.Lambda(x, replaceVar e)
                        | ExprShape.ShapeCombination (shape, args) ->
                            ExprShape.RebuildShapeCombination(shape, args |> List.map replaceVar)
                    mutVar, replaceVar e, replaceVar b

            let tree, body =
                let f ((t, b) as s) ((v, e) as ve, k) =
                    match k with
                    | NotRecursive ->
                        let letTree = toTreeCore e |> continueWith (Tree.Let (v, Tree.Zero))
                        t |> continueWith letTree, b
                    | Recursive ->
                        let v, e, b = makeMutable ve b
                        let initializeTree =
                            Tree.Expr (FsExpr.DefaultValue(v.Type),
                                Tree.Let (v, Tree.Zero))
                        let exprTree = toTreeCore (FsExpr.VarSet(v, e))
                        t |> continueWith (initializeTree |> continueWith exprTree), b
                    | _ -> s
                bindings |> List.fold f (Tree.Zero, body)

            let tree, body, ves =
                let f ((t, b, ves) as s) (ve, k) =
                    match k with
                    | MutuallyRecursive ->
                        let v, e, b = makeMutable ve b
                        let initializeTree =
                            Tree.Expr (FsExpr.DefaultValue(v.Type),
                                Tree.Let (v, Tree.Zero))
                        t |> continueWith initializeTree, b, (v, e) :: ves
                    | _ -> s
                bindings |> List.fold f (tree, body, [])

            let tree =
                let f (v, e) t =
                    t |> continueWith (toTreeCore (FsExpr.VarSet(v, e)))
                List.foldBack f ves tree

            tree |> continueWith (toTreeCore body) |> Tree.reduce

        | Patterns.IfThenElse (guard, thenExpr, elseExpr) ->
            let guardTree = toTreeCore guard
            let thenTree = toTreeCore thenExpr
            let elseTree = toTreeCore elseExpr
            guardTree
            |> continueWith (Tree.If (thenTree, elseTree, Tree.Zero))
            |> Tree.reduce

        | BuilderPatterns.CallWhile builder (guard, body) ->
            let guardTree =
                match guard with
                | Patterns.ValueWithName _ ->
                    // ValueWithName は定数なので DerivedPatterns.Bool にマッチするが
                    // 定数ではなく変数として扱いたいので、先に引っ掛ける。
                    Some (toTreeCore guard)
                | DerivedPatterns.Bool true ->
                    None
                | _ -> Some (toTreeCore guard)                
            let bodyTree = toTreeCore body
            Tree.While (guardTree, bodyTree, Tree.Zero)
            |> Tree.reduce

        | BuilderPatterns.CallBind builder (expr, var, cont) ->
            toTreeCore expr
            |> continueWith (Tree.Bind (letIfNeeded (var, cont)))

        | BuilderPatterns.CallZero builder | DerivedPatterns.Unit ->
            Tree.Zero

        | BuilderPatterns.CallCombine builder (left, right)
        | Patterns.Sequential (left, right) ->
            toTreeCore left |> continueWith (toTreeCore right)

        | BuilderPatterns.CallDelay builder x ->
            toTreeCore x

        | Patterns.Call (Some (Patterns.Value (receiver, _)), method, _)
                when obj.Equals(receiver, builder) ->
            failwithf "Do not call '%s'." method.Name

        | Patterns.VarSet (var, expr) ->
            let setWithLet () =
                let tv = tmpVar ("varSet", var.Type)
                Tree.Let (tv, Tree.Expr (FsExpr.VarSet(var, FsExpr.Var(tv)), Tree.Zero))
            let rec contVarSet = function
                | Tree.Expr (x, Tree.Zero) -> Tree.Expr (FsExpr.VarSet(var, x), Tree.Zero)
                | Tree.Expr (x, y) -> Tree.Expr (x, contVarSet y)
                | Tree.Let (x, y) -> Tree.Let (x, contVarSet y)
                | Tree.If (x, y, Tree.Zero) -> Tree.If (x, y, setWithLet())
                | Tree.If (x, y, z) -> Tree.If (x, y, contVarSet z)
                | Tree.While (x, y, z) -> Tree.While (x, y, contVarSet z)
                | Tree.Bind Tree.Zero -> Tree.Bind (setWithLet())
                | Tree.Bind x -> Tree.Bind (contVarSet x)
                | Tree.Zero -> Tree.Expr (FsExpr.VarSet(var, unitExpr), Tree.Zero)
            toTreeCore expr |> contVarSet

        | Patterns.TryFinally _ -> raise (NotSupportedException("TryFinally"))
        | Patterns.TryWith _ -> raise (NotSupportedException("TryWith"))

        | ExprShape.ShapeCombination (shape, args) ->
            let trees = args |> List.map (fun e -> toTreeCore e, e.Type)

            let exprsOpt =
                let rec f = function
                    | (Tree.SimpleExpr e, _) :: ts ->
                        f ts |> Option.map (fun es -> e :: es)
                    | _ -> None
                f trees

            match exprsOpt with
            | Some exprs ->
                Tree.Expr (ExprShape.RebuildShapeCombination(shape, exprs), Tree.Zero)
            | None ->
                // 簡単に変換できそうにないので、値を変数に退避する
                // TODO: やりすぎ感があるので修正する
                let f (accTree, argExprs) (tree, ty) =
                    let tv = tmpVar ("combArg", ty)
                    let letTree = tree |> continueWith (Tree.Let (tv, Tree.Zero))
                    let accTree = accTree |> continueWith letTree
                    accTree, FsExpr.Var(tv) :: argExprs
                let preludeTree, newArgs = trees |> List.fold f (Tree.Zero, [])
                let expr = ExprShape.RebuildShapeCombination(shape, List.rev newArgs)
                preludeTree |> continueWith (Tree.Expr (expr, Tree.Zero))

        | expr -> Tree.Expr (expr, Tree.Zero)

    and letIfNeeded (var, expr) =
        let tree = toTreeCore expr
        if varIsUsed var expr then Tree.Let (var, tree) else tree

    toTreeCore input.Expr
