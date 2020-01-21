module internal PopDEVS.ProcessOriented.ProcessGraphBuilder

open System
open System.Collections.Generic
open FSharp.Quotations
open FSharpx.Collections
open LetRecUtils
open MutableCfg
open PopDEVS

type private CfgEnv =
    { /// 外部からキャプチャした変数
      CapturedVariables: Dictionary<string, MutableVar>
      /// コンピュテーション式内で定義された変数
      Variables: Dictionary<FsVar, MutableVar> }

let private newEnv() =
    { CapturedVariables = Dictionary()
      Variables = Dictionary() }

let private unitExpr = <@@ () @@>

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
            | Tree.Expr (expr, cont) -> rebuild cont (expr::stack)
            | Tree.Let (var, cont) ->
                let expr = stackToExpr stack
                match rebuild cont [] with
                | Some body -> Some (FsExpr.Let(var, expr, body))
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
            | expr :: stack -> FsExpr.Sequential(stackToExpr stack, expr)
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

type private Terminal =
    { /// 終端処理をしなくても良いか
      CanDiscard: bool
      /// 式を入力し、ノードの式を出力する
      Terminate: FsExpr -> FsExpr<int * WaitCondition option> }

module private Terminal =
    let mkTerminal (canDiscard, terminate) =
        { CanDiscard = canDiscard; Terminate = terminate }

    let terminateOneWay expr =
        FsExpr.Sequential(expr, oneWay) |> excast

    let defaultTerminal = mkTerminal (true, terminateOneWay)
    let oneWayTerminal = mkTerminal (false, terminateOneWay)

open Terminal

let private oneNode node = node, Some node

let build (input: ProcessModelBuilderResult<'I>) =
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
    
            var.FsVar
        | None ->
            // 新規追加
            let fsVar = FsVar(name, varType)
            let newVar = { FsVar = fsVar
                           CapturedValue = Some value }
            env.CapturedVariables.Add(name, newVar)
            env.Variables.Add(fsVar, newVar)
            fsVar

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
                | Tree.If (x, y, cont) | Tree.While (Some x, y, cont) ->
                    f x || f y || f cont
                | Tree.While (None, x, cont) -> f x || f cont
                | Tree.Zero -> false
            f

        match tree with
        | Tree.Expr (_, cont) | Tree.Bind cont ->
            findVars cont
        | Tree.If (x, y, Tree.Let (var, z)) when not ((Tree.lastIsBind x) && (Tree.lastIsBind y)) ->
            // if 式の結果を使うので、代入が必要になるから使用済み判定をする
            env.Variables.Add(var, { FsVar = var; CapturedValue = None })
            findVars x; findVars y; findVars z
        | Tree.If (x, y, z)  | Tree.While (Some x, y, z) ->
            findVars x; findVars y; findVars z
        | Tree.While (None, x, _) ->
            findVars x
        | Tree.Zero -> ()
        | Tree.Let (var, cont) ->
            // 直後でしか使われないなら、ただの let 式にできる
            let isUsed =
                let rec f = function
                    | Tree.Expr (_, x) | Tree.Let (_, x) -> f x
                    | Tree.Zero -> false
                    | x -> usedInTree var x
                f cont
            if isUsed then
                let r = { FsVar = var
                          CapturedValue = None }
                env.Variables.Add(var, r)
            findVars cont

    let rec findCaptured = function
        | Patterns.ValueWithName x ->
            recordCapturedVar x |> FsExpr.Var
        | ExprShape.ShapeVar _ as x -> x
        | ExprShape.ShapeLambda (v, e) -> FsExpr.Lambda(v, findCaptured e)
        | ExprShape.ShapeCombination (shape, args) ->
            ExprShape.RebuildShapeCombination(shape, List.map findCaptured args)

    let newNode p e =
        { LambdaParameter = p
          Expr = e
          IncomingEdges = HashSet()
          OutgoingEdges = List() }

    let createOneWayNode lambdaParameter expr =
        FsExpr.Sequential(expr, oneWay)
        |> excast
        |> newNode lambdaParameter

    let rec createNode terminal =
        let rec withParam p =
            let rec withTree = function
            | Tree.Expr (x, Tree.Expr (y, z)) ->
                withTree (Tree.Expr (FsExpr.Sequential(x, y), z))

            | Tree.Expr (x, Tree.Let (var, cont)) ->
                if env.Variables.ContainsKey(var) then
                    withTree (Tree.Expr (FsExpr.VarSet(var, x), cont))
                else
                    // let 式に変換できると判定された場合、 Variables に登録されない
                    match cont with
                    | Tree.Expr (contExpr, contCont) ->
                        withTree (Tree.Expr (FsExpr.Let(var, x, contExpr), contCont))
                    | _ ->
                        withTree (Tree.Expr (x, cont))

            | Tree.Expr (x, Tree.If (thenTree, elseTree, cont)) ->
                let ifNode =
                    let edgeExpr =
                        // true -> 1, false -> 0
                        FsExpr.IfThenElse(x, FsExpr.Value(1), FsExpr.Value(0))
                        |> FsExpr.Cast<int>
                    <@ %edgeExpr, Option<WaitCondition>.None @>
                    |> newNode p

                let contLeft, contRight, assignVar =
                    match cont with
                    | Tree.Zero when terminal.CanDiscard -> None, None, None
                    | Tree.Let (var, letCont)
                            // 両方が Bind のとき、通常の Let の変形が使えるので特別扱いしなくてよい
                            when not ((Tree.lastIsBind thenTree) && (Tree.lastIsBind elseTree)) ->
                        let a, b = withParam None letCont
                        Some a, b, Some var
                    | _ ->
                        let a, b = withParam None cont
                        Some a, b, None

                let terminate =
                    match assignVar with
                    | Some v -> (fun expr ->
                        (FsExpr.VarSet(v, expr), oneWay)
                        |> FsExpr.Sequential
                        |> excast)
                    | None -> terminateOneWay

                let brTerminal = mkTerminal (Option.isNone contLeft, terminate)
                let createBranch tree =
                    let a, b = createNode brTerminal None tree
                    connectMutNode ifNode a
                    connectOpt b contLeft

                createBranch elseTree // 0
                createBranch thenTree // 1

                ifNode, contRight

            | Tree.Expr (x, Tree.Bind cont) ->
                let node =
                    <@ 0, Some %(FsExpr.Cast<WaitCondition>(x)) @>
                    |> newNode p

                match cont with
                | Tree.Zero -> oneNode node
                | _ ->
                    let p, cont =
                        match cont with
                        | Tree.Let (var, letCont) -> Some var, letCont
                        | _ -> None, cont
                    let a, b = withParam p cont
                    connectMutNode node a
                    node, b

            | Tree.Expr (x, Tree.Zero) ->
                terminal.Terminate x
                |> newNode p
                |> oneNode

            | Tree.Expr (x, (Tree.While _ as cont)) ->
                let node = createOneWayNode p x
                let a, b = withParam None cont
                connectMutNode node a
                node, b

            | Tree.While (None, body, _) -> // infinite loop
                let a, b = createNode oneWayTerminal p body
                connectMutNode (Option.get b) a
                a, if terminal.CanDiscard then None else b

            | Tree.While (Some cond, body, cont) ->
                let condLeft, condRight =
                    let condTerminate expr =
                        let ifExpr =
                            // true -> 0, false -> 1
                            FsExpr.IfThenElse(expr, FsExpr.Value(0), FsExpr.Value(1))
                            |> FsExpr.Cast<int>
                        <@ %ifExpr, Option<WaitCondition>.None @>
                    let t = mkTerminal (false, condTerminate)
                    let x, y = createNode t p cond
                    x, Option.get y

                let bodyLeft, bodyRight =
                    let x, y = createNode oneWayTerminal None body
                    x, Option.get y

                let contLeft, contRight =
                    let cont =
                        match cont with
                        | Tree.If _ | Tree.Bind _ ->
                            failwith "While expression returns an unit value. Cannot continue with If or Bind."
                        | Tree.Let _ -> Some (Tree.Expr (unitExpr, cont))
                        | Tree.Zero when terminal.CanDiscard -> None
                        | _ -> Some cont
                    match cont with
                    | Some cont ->
                        let a, b = withParam None cont
                        Some a, b
                    | None -> None, None
                
                connectMutNode condRight bodyLeft // 0
                Option.iter (connectMutNode condRight) contLeft // 1
                connectMutNode bodyRight condLeft
                condLeft, contRight

            | Tree.Zero ->
                terminal.Terminate unitExpr
                |> newNode p
                |> oneNode

            | Tree.Let _ ->
                failwith "Let can only be placed after Expr, If, While or Bind."
            | Tree.If _ | Tree.Bind _ ->
                failwith "If and Bind can only be placed after Expr."

            withTree
        withParam

    let rootNode, _ = toTree input |> createNode defaultTerminal None
    rootNode
