module internal PopDEVS.ProcessOriented.ProcessGraphBuilder

open System
open System.Collections.Generic
open FSharp.Quotations
open PopDEVS
open IntermediateTree
open PgUtils

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

let private oneNode (node: MutableNode) = node, Some node

let treeToGraph tree =
    let env =
        {| /// 外部からキャプチャした変数
           CapturedVariables = Dictionary<string, ImmutableVar>()
           /// コンピュテーション式内で定義された変数
           Variables = Dictionary<FsVar, ImmutableVar>() |}
    
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
            let newVar: ImmutableVar =
                { FsVar = fsVar; CapturedValue = Some value }
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
        | Tree.If (x, y, z) | Tree.While (Some x, y, z) ->
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
                env.Variables.Add(var, { FsVar = var; CapturedValue = None })
            findVars cont

    let rec findCaptured = function
        | Patterns.ValueWithName x ->
            recordCapturedVar x |> FsExpr.Var
        | ExprShape.ShapeVar _ as x -> x
        | ExprShape.ShapeLambda (v, e) -> FsExpr.Lambda(v, findCaptured e)
        | ExprShape.ShapeCombination (shape, args) ->
            ExprShape.RebuildShapeCombination(shape, List.map findCaptured args)

    let newNode p (e: FsExpr<int * WaitCondition option>) =
        { LambdaParameter = p
          Expr = findCaptured e |> excast
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
                withTree (Tree.Expr (mkSeqExpr x y, z))

            | Tree.Expr (x, Tree.Let (var, cont)) ->
                if env.Variables.ContainsKey(var) then
                    withTree (Tree.Expr (mkVarSet var x, cont))
                else
                    // let 式に変換できると判定された場合、 Variables に登録されない
                    match cont with
                    | Tree.Expr (contExpr, contCont) ->
                        withTree (Tree.Expr (continueWithLet var x contExpr, contCont))
                    | _ ->
                        withTree (Tree.Expr (x, cont))

            | Tree.Expr (x, Tree.If (thenTree, elseTree, cont)) ->
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

                let brTerminal =
                    let terminate =
                        match assignVar with
                        | Some v -> (fun expr ->
                            (FsExpr.VarSet(v, expr), oneWay)
                            |> FsExpr.Sequential
                            |> excast)
                        | None -> terminateOneWay
                    mkTerminal (Option.isNone contLeft, terminate)

                let mkIfThen tree b =
                    let ifNode =
                        let tmap f (x, y) = f x, f y
                        let thenEdge, elseEdge =
                            if b then 0, 1 else 1, 0
                            |> tmap (FsExpr.Value >> FsExpr.Cast<int>)
                        <@ (if %%x then %thenEdge else %elseEdge), Option<WaitCondition>.None @>
                        |> newNode p
                    let a, b = createNode brTerminal None tree
                    connectMutNode ifNode a // 0
                    Option.iter (connectMutNode ifNode) contLeft // 1
                    connectOpt b contLeft
                    ifNode

                let ifNode =
                    match thenTree, elseTree with
                    | body, Tree.Zero -> // then only
                        mkIfThen body true
                    | Tree.Zero, body -> // else only
                        mkIfThen body false
                    | _ ->
                        let ifNode =
                            // true -> 0, false -> 1
                            <@ (if %%x then 0 else 1), Option<WaitCondition>.None @>
                            |> newNode p
                        let createBranch tree =
                            let a, b = createNode brTerminal None tree
                            connectMutNode ifNode a
                            connectOpt b contLeft

                        createBranch thenTree // 0
                        createBranch elseTree // 1
                        ifNode

                ifNode, contRight

            | Tree.Expr (x, Tree.Bind cont) ->
                let node =
                    let convX = FsExpr.Coerce(x, typeof<WaitCondition>)
                                |> FsExpr.Cast<WaitCondition>
                    <@ 0, Some %convX @>
                    |> newNode p

                match cont with
                | Tree.Zero -> oneNode node
                | _ ->
                    let p, cont =
                        match cont with
                        | Tree.Let (var, _) ->
                            let lp = FsVar("waitResult", typeof<obj>)
                            let convExpr = FsExpr.Var(lp) |> unboxExpr var.Type
                            Some lp, Tree.Expr (convExpr, cont)
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

    findVars tree
    let rootNode, _ = createNode defaultTerminal None tree
    createImmutableGraph (env.Variables.Values, rootNode)

let build (input: ProcessModelBuilderResult<'I>) =
    toTree input |> treeToGraph
