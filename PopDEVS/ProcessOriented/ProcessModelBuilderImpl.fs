module PopDEVS.ProcessOriented.ProcessModelBuilderImpl

open System
open System.Collections.Generic
open FSharp.Quotations
open PopDEVS

type internal FsExpr = FSharp.Quotations.Expr
type internal FsExpr<'T> = FSharp.Quotations.Expr<'T>
type internal FsVar = FSharp.Quotations.Var

type Placeholder<'I, 'O, 'V> = struct end

[<ReferenceEquality>]
type internal StateVar =
    { Name: string
      Type: Type
      /// 外部からキャプチャした変数なら、その値を代入
      CapturedValue: obj option
      /// ラムダ式にキャプチャされる変数か
      mutable IsEscaped: bool
      mutable ReferenceCount: int
      mutable Index: int option }

let internal newCapturedVar (name, varType, value) =
    { Name = name
      Type = varType
      CapturedValue = Some value
      IsEscaped = false
      ReferenceCount = 0
      Index = None }

let internal newVar (name, varType) =
    { Name = name
      Type = varType
      CapturedValue = None
      IsEscaped = false
      ReferenceCount = 0
      Index = None }

[<ReferenceEquality>]
type internal CfgNode =    
    { /// 前回のイベントの戻り値を受け取って、処理を行い、次に遷移する辺のインデックスを返す式
      /// 'a -> int * WaitCondition<'I, 'O, 'b> option
      mutable Expr: FsExpr
      mutable ReturnsWaitCondition: bool
      Edges: List<CfgNode> }

let internal printGraph (rootNode: CfgNode) =
    let nodes = List<CfgNode>()
    let rec traverse node =
        if not (nodes.Contains(node)) then
            nodes.Add(node)
            node.Edges |> Seq.iter traverse
    traverse rootNode

    for i, node in Seq.indexed nodes do
        let firstLine =
            sprintf "=== Node %d%s ===" i
                (if node.ReturnsWaitCondition then " (Wait)" else "")
        let edgeNumbers = node.Edges |> Seq.map (fun n -> nodes.IndexOf(n))
        printf "%s\n%A\nEdges: %s\n%s\n\n"
            firstLine
            node.Expr
            (String.Join(", ", edgeNumbers))
            (String('=', firstLine.Length))

/// CfgNode.Expr の戻り値の種類
type internal NodeBehavior =
    /// 何も返さない（次に遷移する辺だけ指定する）
    | Unit
    /// 値を変数に代入したあと、 Unit と同じ挙動をする
    | Assign of FsVar
    /// WaitCondition を返す
    | WaitCondition

type internal NodeOrExpr =
    | Node of (CfgNode * CfgNode)
    | Expr of FsExpr

let internal newNode (expr, returnsWaitCondition) =
    { Expr = expr
      Edges = List<CfgNode>()
      ReturnsWaitCondition = returnsWaitCondition }

type internal CfgEnv =
    { /// 外部からキャプチャした変数
      CapturedVariables: Dictionary<string, StateVar>
      /// コンピュテーション式内で定義された変数
      Variables: Dictionary<FsVar, StateVar> }

let internal newEnv () =
    { CapturedVariables = new Dictionary<string, StateVar>()
      Variables = new Dictionary<FsVar, StateVar>() }

type BuilderResult<'I, 'O> internal () = class end

type Builder<'I, 'O>() =
    let doNotCall () =
        invalidOp "Do not call methods of Builder from your code."

    member __.Bind<'a, 'b>(_computation: WaitCondition<'I, 'a>, _binder: 'a -> 'b) : 'b =
        doNotCall ()

    member __.Combine<'a>(_left: unit, _right: 'a) : 'a =
        doNotCall ()

    member __.While(_guard: unit -> bool, _computation: unit) : unit =
        doNotCall ()

    member __.Zero() : unit =
        doNotCall ()

    member __.Delay<'a>(_: unit -> 'a) : 'a =
        doNotCall ()

    member __.Quote(_: Expr<unit>) : Expr<unit> =
        doNotCall ()

    member this.Run(expr: Expr<unit>) =
        let env = newEnv ()

        /// 出現した ValueWithName を env に記録する
        let recordCapturedVar (value, varType, name) =
            if isNull varType then nullArg "varType"
            if String.IsNullOrEmpty(name) then invalidArg "name" "name is null or empty."

            match env.CapturedVariables.TryFind(name) with
            | Some var ->
                // すでに記録されているので、アサーション
                if not (obj.Equals(varType, var.Type)) then
                    failwithf "Type mismatch (Expected: %O, Actual: %O)" var.Type varType

                let existingValue = var.CapturedValue.Value
                if not (obj.Equals(value, existingValue)) then
                    failwithf "Value mismatch (Expected: %O, Actual: %O)" existingValue value

            | None ->
                // 新規追加
                env.CapturedVariables.Add(name, newCapturedVar (name, varType, value))

        /// let された変数を env に追加する
        let addVar (var: FsVar) =
            env.Variables.Add(var, newVar (var.Name, var.Type))

        let markAsEscaped (var: FsVar) =
            match env.Variables.TryFind(var) with
            | Some x -> x.IsEscaped <- true
            | None -> ()

        let connect (left, right) = left.Edges.Add(right)

        let oneWay = <@ 0, None @>
        let boxExpr expr = FsExpr.Cast<obj>(FsExpr.Coerce(expr, typeof<obj>))
        let discardObjVar () = FsVar("_", typeof<obj>)

        let (|OneWayExpr|_|) = function
            | Patterns.NewTuple [DerivedPatterns.Int32 0; Patterns.NewUnionCase (caseInfo, [])]
                when caseInfo.DeclaringType = typeof<obj option> && caseInfo.Name = "None" ->
                Some ()
            | _ -> None

        let (|OneWayLambda|_|) expr =
            /// Sequential で連結された最後の式と、それ以外に分離する
            let rec splitLastExpr = function
                | Patterns.Sequential (left, right) ->
                    match splitLastExpr right with
                    | Some proc, last -> Some (FsExpr.Sequential (left, proc)), last
                    | None, last -> Some left, last
                | x -> None, x

            match expr with
            | Patterns.Lambda (lambdaVar, lambdaBody) ->
                let body, last = splitLastExpr lambdaBody
                match last with
                | OneWayExpr -> Some (lambdaVar, body)
                | _ -> None
            | _ -> None
        
        /// node の最後に expr を挿入する
        let appendExpr expr node =
            match expr, node.Expr with
            | DerivedPatterns.Unit, _ -> ()
            | _, OneWayLambda (lambdaVar, body) ->
                let newBody =
                    match body with
                    | None | Some DerivedPatterns.Unit -> expr
                    | Some x -> FsExpr.Sequential(x, expr)
                node.Expr <- FsExpr.Lambda(lambdaVar, FsExpr.Sequential(newBody, oneWay))
            | _ -> invalidArg "node" "node.Expr is not one way expression."

        // TODO: while の条件ノードに挿入できてしまうバグがある
        /// node の最初に expr を挿入する
        let prependExpr expr node =
            match expr, node.Expr with
            | DerivedPatterns.Unit, _ -> ()
            | _, OneWayLambda (lambdaVar, (None | Some DerivedPatterns.Unit)) ->
                node.Expr <- FsExpr.Lambda(lambdaVar, FsExpr.Sequential(expr, oneWay))
            | _, Patterns.Lambda (lambdaVar, body) ->
                node.Expr <- FsExpr.Lambda(lambdaVar, FsExpr.Sequential(expr, body))
            | _ -> invalidArg "node" "node.Expr is not a lambda."

        /// 2つの NodeOrExpr を連結する
        let connectNodeOrExpr = function
            | Expr DerivedPatterns.Unit, Expr right ->
                Expr right
            | Expr left, Expr DerivedPatterns.Unit ->
                Expr left
            | Expr left, Expr right ->
                Expr (FsExpr.Sequential(left, right))
            | (Node (_, leftLast) as left), Expr right ->
                appendExpr right leftLast
                left
            | Expr left, (Node (rightFirst, _) as right) ->
                prependExpr left rightFirst
                right
            | Node (leftFirst, leftLast), Node (rightFirst, rightLast) ->
                connect (leftLast, rightFirst)
                Node (leftFirst, rightLast)

        /// 条件を実行し、 false なら 0 番目、 true なら 1 番目の辺に進むノードを作成する
        let condToNode cond =
            // TODO: cond 自体が分岐することも考える
            // fun _ -> (if cond then 1 else 0), None
            let transitionExpr = FsExpr.IfThenElse(cond, <@@ 1 @@>, <@@ 0 @@>)
            let condLambda = FsExpr.Lambda(discardObjVar (), FsExpr.NewTuple([transitionExpr; <@@ None @@>]))
            newNode (condLambda, false)

        let doNothingNode () =
            newNode (FsExpr.Lambda(discardObjVar (), oneWay), false)

        let ensureWaitConditionType (ty: Type) =
            if ty.FullName <> "PopDEVS.ProcessOriented.WaitCondition`2" then
                failwith "The type of the expression is not WaitCondition<'I, 'R>."

        let exprToNode (expr, kind) =
            match kind with
            | Unit ->
                // fun _ ->
                //     expr
                //     0, None
                let lambda = FsExpr.Lambda(discardObjVar (), FsExpr.Sequential(expr, oneWay))
                let node = newNode (lambda, false)
                node
            | Assign var ->
                // fun _ ->
                //     var <- expr
                //     0, None
                let lambda =
                    FsExpr.Lambda(
                        discardObjVar (),
                        FsExpr.Sequential(
                            FsExpr.VarSet(var, expr),
                            oneWay))
                let node = newNode (lambda, false)
                node
            | WaitCondition ->
                let node = newNode (<@ fun _ -> 0, Some %(boxExpr expr) @>, true)
                node

        let rec createCfg (expr, kind) =
            match createCfgOrExpr (expr, kind) with
            | Node nodes -> nodes
            | Expr expr ->
                // createCfgOrExpr が Expr を返してきたなら、 CfgNode に変換
                let node = exprToNode (expr, kind)
                node, node

        and createCfgOrExpr (expr, kind) : NodeOrExpr =
            match expr with
            // Builder のメソッド呼び出し

            | DerivedPatterns.SpecificCall <@@ this.Delay @@> (_, _, [generator]) ->
                match generator with
                | Patterns.Lambda (_, body) -> createCfgOrExpr (body, kind)
                | _ -> failwith "The argument of the Delay call is not a lambda."

            | DerivedPatterns.SpecificCall <@@ this.Bind @@> (_, _, [arg1; arg2]) ->
                // WaitCondition<'Input, 'Result> の 'Result を取得
                let waitResultType = arg1.Type.GetGenericArguments().[1]

                // Bind 前のノードからの継続として、 Bind 後のノードを接続
                let leftFirst, leftLast = createCfg (arg1, WaitCondition)

                match arg2 with
                | Patterns.Lambda (resultVar, body) ->
                    // resultVar が使われているなら、 resultVar に結果を代入するノードを作る。
                    // そうでないなら、結果を無視して、単純なノードを作る。
                    if body.GetFreeVars() |> Seq.contains resultVar then
                        addVar resultVar

                        let funcParam = FsVar("waitResult", typeof<obj>)
                        let assignExpr = FsExpr.VarSet(resultVar, FsExpr.Coerce(FsExpr.Var(funcParam), waitResultType))

                        match createCfgOrExpr (body, kind) with
                        | Node (rightFirst, rightLast) ->
                            // rightFirst の前に、代入する式を挿入する
                            match rightFirst.Expr with
                            | Patterns.Lambda (lambdaVar, rightFirstBody) ->
                                if rightFirstBody.GetFreeVars() |> Seq.contains lambdaVar then
                                    failwith "The lambda parameter is referenced."
                                rightFirst.Expr <-
                                    FsExpr.Lambda(funcParam, FsExpr.Sequential(assignExpr, rightFirstBody))
                            | _ -> failwith "rightFirst.Expr is not a lambda."

                            connect (leftLast, rightFirst)
                            Node (leftFirst, rightLast)

                        | Expr bodyExpr ->
                            let nodeExpr, returnsWaitCondition =
                                match kind with
                                | Unit ->
                                    // fun waitResult ->
                                    //     resultVar <- waitResult
                                    //     bodyExpr
                                    //     0, None
                                    let e =
                                        FsExpr.Sequential(
                                            FsExpr.Sequential(assignExpr, bodyExpr),
                                            oneWay)
                                    e, false
                                | Assign varToAssign ->
                                    // fun waitResult ->
                                    //     resultVar <- waitResult
                                    //     varToAssign <- bodyExpr
                                    //     0, None
                                    let e =
                                        FsExpr.Sequential(
                                            FsExpr.Sequential(
                                                assignExpr,
                                                FsExpr.VarSet(varToAssign, bodyExpr)),
                                            oneWay)
                                    e, false
                                | WaitCondition ->
                                    // fun waitResult ->
                                    //     resultVar <- waitResult
                                    //     0, Some bodyExpr
                                    let e =
                                        FsExpr.Sequential(
                                            assignExpr,
                                            <@ 0, Some %(boxExpr bodyExpr) @>)
                                    e, true
                            let lambda = FsExpr.Lambda(funcParam, nodeExpr)
                            let rightNode = newNode (lambda, returnsWaitCondition)
                            connect (leftLast, rightNode)
                            Node (leftLast, rightNode)
                    else
                        let rightFirst, rightLast = createCfg (body, kind)
                        connect (leftLast, rightFirst)
                        Node (leftLast, rightLast)

                | _ -> failwith "The second argument of the Bind call is not a lambda."

            | DerivedPatterns.SpecificCall <@@ this.Combine @@> (_, _, [arg1; arg2]) ->
                transCombine (arg1, arg2, kind)

            | DerivedPatterns.SpecificCall <@@ this.While @@> (_, _, [arg1; arg2]) ->
                match arg1 with
                | Patterns.Lambda (_, cond) ->
                    match kind with
                    | Unit -> ()
                    | _ -> failwith "NodeBehavior.Unit can only be used for While."

                    let testNode = condToNode cond
                    let lastNode = doNothingNode ()
                    let (bodyFirst, bodyLast) = createCfg (arg2, Unit)

                    connect (testNode, lastNode)
                    connect (testNode, bodyFirst)
                    connect (bodyLast, testNode)

                    Node (testNode, lastNode)

                | _ -> failwith "The first argument of the While call is not a lambda."

            | DerivedPatterns.SpecificCall <@@ this.Zero @@> _ ->
                Expr <@@ () @@>

            | Patterns.Call (Some (Patterns.Value (receiver, _)), method, _)
                when obj.ReferenceEquals(receiver, this) ->
                failwithf "Do not call '%s'." method.Name

            // その他の文法

            | Patterns.IfThenElse (cond, trueExpr, falseExpr) ->
                let testNode = condToNode cond

                let (trueFirst, trueLast), (falseFirst, falseLast), joinNode =
                    match kind with
                    | Unit | Assign _ ->
                        // true, false それぞれで、やることをやって、最後に何もしないノードに戻ってくる
                        createCfg (trueExpr, kind), createCfg (falseExpr, kind), doNothingNode ()

                    | WaitCondition ->
                        let trueType, falseType = trueExpr.Type, falseExpr.Type
                        if not (obj.Equals(trueType, falseType)) then
                            failwithf "Type mismatch (True: %O, False: %O)" trueType falseType
                        ensureWaitConditionType trueType

                        // trueExpr, falseExpr が返す WaitCondition を変数に保存して、 joinNode でそれを返す
                        // TODO: WaitCondition を状態保存に含めるわけにはいかないので、うまく変数を使うのを回避する
                        let waitCondVar = FsVar("waitCondition", trueType)
                        addVar waitCondVar
                        let trueTuple = createCfg (trueExpr, Assign waitCondVar)
                        let falseTuple = createCfg (falseExpr, Assign waitCondVar)
                        let joinNode =
                            let varExpr = boxExpr (FsExpr.Var(waitCondVar))
                            let lambda = <@ fun _ -> 0, Some %varExpr @>
                            newNode (lambda, true)
                        trueTuple, falseTuple, joinNode

                connect (testNode, falseFirst)
                connect (falseLast, joinNode)
                connect (testNode, trueFirst)
                connect (trueLast, joinNode)
                Node (testNode, joinNode)

            | Patterns.Let (var, value, body) -> transLet ([(var, value)], body, kind)
            | Patterns.LetRecursive (bindings, body) -> transLet (bindings, body, kind)

            | Patterns.Sequential (left, right) -> transCombine (left, right, kind)

            | Patterns.ValueWithName x as expr ->
                recordCapturedVar x
                Expr expr

            | Patterns.TryFinally _ -> raise (new NotSupportedException("TryFinally"))
            | Patterns.TryWith _ -> raise (new NotSupportedException("TryWith"))
            | Patterns.WhileLoop _ -> raise (new NotSupportedException("WhileLoop"))

            | ExprShape.ShapeVar var -> Expr (FsExpr.Var(var))

            | ExprShape.ShapeLambda (var, expr) ->
                expr.GetFreeVars() |> Seq.iter markAsEscaped
                Expr (FsExpr.Lambda(var, expr))

            | ExprShape.ShapeCombination (shape, args) ->
                let blocks = List()
                let currentBlock = List()
                for argExpr in args do
                    match createCfgOrExpr (argExpr, Unit) with
                    | Node x ->
                        blocks.Add((currentBlock.ToArray(), x))
                        currentBlock.Clear()
                    | Expr x -> currentBlock.Add(x)

                if blocks.Count = 0 then
                    // CFG ノードへの変形は必要ないので、そのまま再構成する
                    Expr (ExprShape.RebuildShapeCombination(shape, List.ofSeq currentBlock))
                else
                    raise (NotImplementedException()) // TODO

        and transLet (bindings, body, kind) =
            let rec bindingToNode = function
                | [(var, value)] ->
                    addVar var
                    match createCfgOrExpr (value, Assign var) with
                    | Node _ as x -> x
                    | Expr expr ->
                        // Expr の場合、まだ VarSet されていないので、 VarSet する
                        Expr (FsExpr.VarSet(var, expr))

                | (var, value) :: xs ->
                    addVar var
                    let left =
                        match createCfgOrExpr (value, Assign var) with
                        | Node _ as x -> x
                        | Expr expr -> Expr (FsExpr.VarSet(var, expr))
                    let right = bindingToNode xs
                    connectNodeOrExpr (left, right)

                | [] -> invalidArg "bindings" "bindings is empty."

            connectNodeOrExpr (bindingToNode bindings, createCfgOrExpr (body, kind))

        and transCombine (left, right, kind) =
            connectNodeOrExpr (createCfgOrExpr (left, Unit), createCfgOrExpr (right, kind))

        let rootNode, exitNode = createCfg (expr, Unit)
        printGraph rootNode

        raise (NotImplementedException())
        //BuilderResult<'I, 'O>(tree)
