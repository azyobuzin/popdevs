module internal PopDEVS.ProcessOriented.ProcessGraphBuilder

open System
open System.Collections.Generic
open FSharp.Quotations
open MutableCfg

type private CfgEnv =
    { /// 外部からキャプチャした変数
      CapturedVariables: Dictionary<string, MutableVar>
      /// コンピュテーション式内で定義された変数
      Variables: Dictionary<FsVar, MutableVar> }

[<RequireQualifiedAccess>]
type private Tree =
    | Expr of FsExpr * cont: Tree
    | Let of FsVar * cont: Tree
    | LetRec of (FsVar * FsExpr) list * cont: Tree
    | If of t: Tree * f: Tree * cont: Tree
    | While of cond: Tree * body: Tree * cont: Tree
    | Bind of cont: Tree
    | Zero

let private (|SimpleExpr|_|) = function
    | Tree.Expr (expr, Tree.Zero) -> Some expr
    | _ -> None

let private isSimpleExpr = function
    | SimpleExpr _ -> true
    | _ -> false

let private unitExpr = <@@ () @@>

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

let rec private continueWith cont tree =
    let rec f = function
        | Tree.Expr (x, Tree.Zero) ->
            match cont with
            | Tree.Expr (y, z) -> Tree.Expr (FsExpr.Sequential(x, y), z)
            | _ -> Tree.Expr (x, cont)
        | Tree.Expr (x, y) -> Tree.Expr (x, f y)
        | Tree.Let (x, y) -> Tree.Let (x, f y)
        | Tree.LetRec (x, y) -> Tree.LetRec (x, f y)
        | Tree.If (x, y, z) -> Tree.If (x, y, f z)
        | Tree.While (x, y, z) -> Tree.While (x, y, f z)
        | Tree.Bind x -> Tree.Bind (f x)
        | Tree.Zero -> cont
    f tree

/// `Tree.Bind` が含まれない `Tree` ならば、 `FsExpr` を作成する
let rec private rebuildExpr tree =
    let rec rebuild tree stack =
        match tree with
        | Tree.Expr (expr, cont) -> rebuild cont (expr::stack)
        | Tree.Let (var, cont) ->
            let expr = stackToExpr stack
            match rebuild cont [] with
            | Some body -> Some (FsExpr.Let(var, expr, body))
            | None -> None
        | Tree.LetRec (bindings, cont) ->
            let expr = stackToExpr stack
            match rebuild cont [] with
            | Some body ->
                Some (
                    let letRecExpr = FsExpr.LetRecursive(bindings, body)
                    match expr with
                    | DerivedPatterns.Unit -> letRecExpr
                    | _ -> FsExpr.Sequential(expr, letRecExpr))
            | None -> None
        | Tree.If (t, f, cont) ->
            match rebuildExpr t, rebuildExpr f with
            | Some t, Some f ->
                let condExpr = stackToExpr stack
                rebuild cont [FsExpr.IfThenElse(condExpr, t, f)]
            | _ -> None
        | Tree.While (cond, body, cont) ->
            match rebuildExpr cond, rebuildExpr body with
            | Some cond, Some body -> rebuild cont [FsExpr.WhileLoop(cond, body)]
            | _ -> None
        | Tree.Bind _ -> None
        | Tree.Zero -> Some (stackToExpr stack)
    and stackToExpr = function
        | [] -> unitExpr
        | [expr] -> expr
        | expr::stack -> FsExpr.Sequential(stackToExpr stack, expr)
    rebuild tree []

let private reduceTree tree =
    match rebuildExpr tree with
    | Some expr -> Tree.Expr (expr, Tree.Zero)
    | None -> tree

let toTree (input: ProcessModelBuilderResult<'I>) =
    let builder = input.Builder

    let mutable tmpVarCount = 0
    let tmpVar (name, ty) =
        tmpVarCount <- tmpVarCount + 1
        FsVar(sprintf "%s%d" name tmpVarCount, ty)

    let rec toTreeCore = function
        | Patterns.Let (var, expr, body) ->
            toTreeCore expr
            |> continueWith (letIfNeeded (var, body))
            |> reduceTree

        | Patterns.LetRecursive (bindings, body) ->
            if bindings |> List.exists (snd >> toTreeCore >> isSimpleExpr) then
                failwith "Too complex let rec"

            Tree.LetRec (bindings, toTreeCore body)
            |> reduceTree

        | Patterns.IfThenElse (guard, thenExpr, elseExpr) ->
            let guardTree = toTreeCore guard
            let thenTree = toTreeCore thenExpr
            let elseTree = toTreeCore elseExpr
            guardTree
            |> continueWith (Tree.If (thenTree, elseTree, Tree.Zero))
            |> reduceTree

        | BuilderPatterns.CallWhile builder (guard, body) ->
            let guardTree = toTreeCore guard
            let bodyTree = toTreeCore body
            Tree.While (guardTree, bodyTree, Tree.Zero)
            |> reduceTree

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
                | Tree.LetRec (x, y) -> Tree.LetRec (x, contVarSet y)
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
                    | (SimpleExpr e, _) :: ts ->
                        f ts |> Option.map (fun es -> e :: es)
                    | _ -> None
                f trees

            match exprsOpt with
            | Some exprs ->
                Tree.Expr (ExprShape.RebuildShapeCombination(shape, exprs), Tree.Zero)
            | None ->
                // 簡単に変換できそうにないので、値を変数に退避する
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

let build (input: ProcessModelBuilderResult<'I>) =
    let builder = input.Builder
    let env = newEnv ()
    
    /// 出現した ValueWithName を env に記録する
    let recordCapturedVar (value, varType, name) =
        if isNull varType then nullArg (nameof varType)
        if String.IsNullOrEmpty(name) then invalidArg (nameof name) "name is null or empty."
    
        match RoDic.tryFind name env.CapturedVariables with
        | Some var ->
            // すでに記録されているので、アサーション
            if not (obj.Equals(varType, var.FsVar.Type)) then
                failwithf "Type mismatch (Expected: %O, Actual: %O)" var.FsVar.Type varType
    
            let existingValue = var.CapturedValue.Value
            if not (obj.Equals(value, existingValue)) then
                failwithf "Value mismatch (Expected: %O, Actual: %O)" existingValue value
    
            var
        | None ->
            // 新規追加
            let fsVar = FsVar(name, varType)
            let newVar = { FsVar = fsVar
                           CapturedValue = Some value }
            env.CapturedVariables.Add(name, newVar)
            env.Variables.Add(fsVar, newVar)
            newVar

    let rec findVars tree =
        let rec usedInExpr var =
            let rec f = function
                | ExprShape.ShapeVar x -> x = var
                | ExprShape.ShapeLambda (_, x) -> f x
                | ExprShape.ShapeCombination (_, exprs) ->
                    exprs |> List.exists f
            f
        and usedInTree var =
            let rec f = function
                | Tree.Expr (expr, cont) -> usedInExpr var expr || f cont
                | Tree.Let (_, cont) | Tree.Bind cont -> f cont
                | Tree.If (x, y, cont) | Tree.While (x, y, cont) ->
                    f x || f y || f cont
                | Tree.Zero -> false
            f

        match tree with
        | Tree.Expr (_, cont) | Tree.Bind cont ->
            findVars cont
        | Tree.If (x, y, z)  | Tree.While (x, y, z) ->
            findVars x; findVars y; findVars z
        | Tree.Zero -> ()
        | Tree.Let (var, cont) ->
            // 直後でしか使われないなら、ただの let 式にできる
            let isUsed =
                let rec f = function
                    | Tree.Expr (_, x) | Tree.Let (_, x) | Tree.LetRec (_, x) -> f x
                    | Tree.Zero -> false
                    | x -> usedInTree x
                f cont
            if isUsed then
                let r = { FsVar = var
                          CapturedValue = None }
                env.Variables.Add(var, r)

    let rec createNode = function
        | Tree.Expr (x, Tree.Expr (y, z)) ->
            createNode (Tree.Expr (FsExpr.Sequential(x, y), z))

        | Tree.Expr (x, Tree.Let (var, cont)) ->
            if env.Variables.ContainsKey(var) then
                createNode (Tree.Expr (FsExpr.VarSet(var, x), cont))
            else
                match cont with
                | Expr.Tree (contExpr, contCont) ->
                    createNode (Tree.Expr (FsExpr.Let(var, x, contExpr), contCont))
                | _ ->
                    createNode (Tree.Expr (x, cont))

        // TODO
