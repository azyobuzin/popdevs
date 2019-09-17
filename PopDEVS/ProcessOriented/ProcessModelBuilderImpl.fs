module PopDEVS.ProcessOriented.ProcessModelBuilderImpl

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Text
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
            String.Format(
                "=== Node {0}{1} ===",
                i,
                if node.ReturnsWaitCondition then " (Wait)" else "")
        printfn "%s\n%A" firstLine node.Expr
        let edgeNumbers = node.Edges |> Seq.map (fun n -> nodes.IndexOf(n))
        printfn "Edges: %s" (String.Join(", ", edgeNumbers))
        printfn "%s\n" (String('=', firstLine.Length))

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

        let connect (left, right) = left.Edges.Add(right)

        let oneWay = <@ 0, None @>
        let boxExpr expr = FsExpr.Cast<obj>(FsExpr.Coerce(expr, typeof<obj>))
        let discardObjVar () = FsVar("_", typeof<obj>)

        /// 条件を実行し、 false なら 0 番目、 true なら 1 番目の辺に進むノードを作成する
        let condToNode cond =
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
                    addVar resultVar

                    let funcParam = FsVar("waitResult", typeof<obj>)
                    let assignExpr = FsExpr.VarSet(resultVar, FsExpr.Coerce(FsExpr.Var(funcParam), waitResultType))

                    match createCfgOrExpr (body, kind) with
                    | Node (rightFirst, rightLast) ->
                        let assignNode =
                            // fun waitResult ->
                            //     resultVar <- waitResult
                            //     0, None
                            let assignLambda = FsExpr.Lambda(funcParam, FsExpr.Sequential(assignExpr, oneWay))
                            newNode (assignLambda, false)
                        connect (leftLast, assignNode)
                        connect (assignNode, rightFirst)
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

            | Patterns.TryFinally _ -> raise (new NotSupportedException("TryFinally"))
            | Patterns.TryWith _ -> raise (new NotSupportedException("TryWith"))
            | Patterns.WhileLoop _ -> raise (new NotSupportedException("WhileLoop"))

            // TODO: もっとちゃんと再帰的に見る
            | expr -> Expr expr

        and transLet (bindings, body, kind) =
            let rec bindingToNode = function
                | [(var, value)] ->
                    match createCfgOrExpr (value, Assign var) with
                    | Node _ as x -> x
                    | Expr expr ->
                        // Expr の場合、まだ VarSet されていないので、 VarSet する
                        Expr (FsExpr.VarSet(var, expr))

                | (var, value) :: xs ->
                    match bindingToNode xs with
                    | Node (rightFirst, rightLast) ->
                        // TODO: left が Expr で済むなら、前のノードとまとめる
                        let leftFirst, leftLast = createCfg (value, Assign var)
                        connect (leftLast, rightFirst)
                        Node (leftFirst, rightLast)

                    | Expr rightExpr ->
                        match createCfgOrExpr (value, Assign var) with
                        | Node (leftFirst, leftLast) ->
                            let rightNode = exprToNode (rightExpr, Unit)
                            connect (leftLast, rightNode)
                            Node (leftFirst, rightNode)
                        | Expr leftExpr ->
                            Expr (FsExpr.Sequential(FsExpr.VarSet(var, leftExpr), rightExpr))

                | [] -> invalidArg "bindings" "bindings is empty."

            match bindingToNode bindings with
            | Node (bindingFirst, bindingLast) ->
                // TODO: body が Expr で済むなら、前のノードとまとめる
                let bodyFirst, bodyLast = createCfg (body, kind)
                connect (bindingLast, bodyFirst)
                Node (bindingFirst, bodyLast)
            | Expr bindingExpr ->
                match createCfgOrExpr (body, kind) with
                | Node (bodyFirst, bodyLast) ->
                    let bindingNode = exprToNode (bindingExpr, Unit)
                    connect (bindingNode, bodyFirst)
                    Node (bindingNode, bodyLast)
                | Expr bodyExpr ->
                    Expr (FsExpr.Sequential(bindingExpr, bodyExpr))

        and transCombine (left, right, kind) =
            match createCfgOrExpr (left, Unit), createCfgOrExpr (right, kind) with
            | Expr left, Expr right ->
                Expr (FsExpr.Sequential(left, right))
            | t ->
                let (leftFirst, leftLast), (rightFirst, rightLast) =
                    match t with
                    | Node left, Expr right ->
                        let rightNode = exprToNode (right, kind)
                        left, (rightNode, rightNode)
                    | Expr left, Node right ->
                        let leftNode = exprToNode (left, Unit)
                        (leftNode, leftNode), right
                    | Node left, Node right -> left, right
                    | Expr _, Expr _ -> failwith "unreachable"
                connect (leftLast, rightFirst)
                Node (leftFirst, rightLast)

        let rootNode, exitNode = createCfg (expr, Unit)
        printGraph rootNode

        raise (NotImplementedException())
        //BuilderResult<'I, 'O>(tree)
