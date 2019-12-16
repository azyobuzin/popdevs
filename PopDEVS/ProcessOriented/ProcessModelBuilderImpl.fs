module PopDEVS.ProcessOriented.ProcessModelBuilderImpl

open System
open System.Collections.Generic
open FSharp.Quotations
open FSharp.Reflection
open PopDEVS
open MutableCfg

let private newNode (param: FsVar, expr, returnsWaitCondition) =
    if not (obj.Equals(param.Type, typeof<obj>)) then
        invalidArg (nameof param) "param.Type is not obj"

    { LambdaParameter = param
      Expr = expr |> excast
      Edges = List<MutableNode>()
      HasMultipleIncomingEdges = false
      ReturnsWaitCondition = returnsWaitCondition }

/// CfgNode.Expr の戻り値の種類
type internal NodeBehavior =
    /// 何も返さない（次に遷移する辺だけ指定する）
    | Unit
    /// 値を変数に代入したあと、 Unit と同じ挙動をする
    | Assign of FsVar
    /// WaitCondition を返す
    | WaitCondition

type internal NodeOrExpr =
    | Node of (MutableNode * MutableNode)
    | Expr of FsExpr

type internal CfgEnv =
    { /// 外部からキャプチャした変数
      CapturedVariables: Dictionary<string, MutableVar>
      /// コンピュテーション式内で定義された変数
      Variables: Dictionary<FsVar, MutableVar> }

let internal newEnv () =
    { CapturedVariables = new Dictionary<string, MutableVar>()
      Variables = new Dictionary<FsVar, MutableVar>() }

type Builder<'I>() =
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

    /// <summary><paramref name="expr" /> から制御フローグラフを作成します。</summary>
    member this.Run(expr: Expr<unit>) =
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
                               CapturedValue = Some value
                               IsEscaped = false }
                env.CapturedVariables.Add(name, newVar)
                env.Variables.Add(fsVar, newVar)
                newVar

        /// let された変数を env に追加する
        let addVar (var: FsVar) =
            let x = { FsVar = var
                      CapturedValue = None
                      IsEscaped = false }
            env.Variables.Add(var, x)

        let markAsEscaped (var: FsVar) =
            match RoDic.tryFind var env.Variables with
            | Some x -> x.IsEscaped <- true
            | None -> ()

        let oneWay = <@ 0, Option<WaitCondition>.None @>
        let waitCondExpr expr = FsExpr.Cast<WaitCondition>(FsExpr.Coerce(expr, typeof<WaitCondition>))
        let discardObjVar () = FsVar("_", typeof<obj>)
        let mutable tmpVarCount = 0
        let tmpVar (name, ty) =
            tmpVarCount <- tmpVarCount + 1
            FsVar(sprintf "%s%d" name tmpVarCount, ty)

        let exprToNode (expr, kind) =
            match kind with
            | Unit ->
                // fun _ ->
                //     expr
                //     0, None
                let body = FsExpr.Sequential(expr, oneWay)
                let node = newNode (discardObjVar(), body, false)
                node
            | Assign var ->
                // fun _ ->
                //     var <- expr
                //     0, None
                let body = FsExpr.Sequential(FsExpr.VarSet(var, expr), oneWay)
                let node = newNode (discardObjVar(), body, false)
                node
            | WaitCondition ->
                let node = newNode (discardObjVar(), <@ 0, Some %(waitCondExpr expr) @>, true)
                node

        /// node の最後に expr を挿入する
        let appendExpr expr node =
            match expr, node.Expr with
            | DerivedPatterns.Unit, _ -> ()
            | _, OneWayBody body ->
                let newBody =
                    match body with
                    | None | Some DerivedPatterns.Unit -> expr
                    | Some x -> FsExpr.Sequential(x, expr)
                node.Expr <- FsExpr.Sequential(newBody, oneWay) |> excast
            | _ -> invalidArg (nameof node) "node.Expr is not a OneWayBody."

        /// node の最初に expr を挿入する。
        /// 挿入できる条件を満たさない場合は、ノードを作成する。
        let tryPrependExpr expr node =
            if node.HasMultipleIncomingEdges then
                // 複数の入力辺を持つ（ループ）場合、このノードを操作すると
                // ループが破壊されるので、新たにノードを作成する。
                let leftNode = exprToNode (expr, Unit)
                connectMutNode (leftNode, node)
                leftNode
            else
                match expr, node.Expr with
                    | DerivedPatterns.Unit, _ -> ()
                    | _, OneWayBody (None | Some DerivedPatterns.Unit) ->
                        node.Expr <- FsExpr.Sequential(expr, oneWay) |> excast
                    | _, x ->
                        node.Expr <- FsExpr.Sequential(expr, x) |> excast
                node

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
            | Expr left, Node (rightFirst, rightLast) ->
                let first = tryPrependExpr left rightFirst
                Node (first, rightLast)
            | Node (leftFirst, leftLast), Node (rightFirst, rightLast) ->
                connectMutNode (leftLast, rightFirst)
                Node (leftFirst, rightLast)

        let doNothingNode () =
            newNode (discardObjVar(), oneWay, false)

        let ensureWaitConditionType (ty: Type) =
            let b = ty.IsGenericType
            if b && ty.GetGenericTypeDefinition().FullName <> "PopDEVS.ProcessOriented.WaitCondition`2" then
                failwith "The type of the expression is not WaitCondition<'I, 'R>."

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
                    // (fun _arg1 -> let x = _arg1 in ...) の形の場合、不要な代入を取り除く
                    let resultVar, body =
                        match body with
                        | Patterns.Let (letVar, Patterns.Var bindingVar, inExpr)
                            when bindingVar = resultVar && not (inExpr.GetFreeVars() |> Seq.contains resultVar) ->
                            letVar, inExpr
                        | _ -> resultVar, body

                    // resultVar が使われているなら、 resultVar に結果を代入するノードを作る。
                    // そうでないなら、結果を無視して、単純なノードを作る。
                    if body.GetFreeVars() |> Seq.contains resultVar then
                        addVar resultVar

                        let funcParam = tmpVar ("waitResult", typeof<obj>)
                        let assignExpr = FsExpr.VarSet(resultVar, FsExpr.Var(funcParam) |> unboxExpr waitResultType)

                        match createCfgOrExpr (body, kind) with
                        | Node (rightFirst, rightLast) ->
                            if refToParam rightFirst then
                                failwith "The lambda parameter is referenced."

                            // rightFirst の前に、代入する式を挿入する
                            rightFirst.LambdaParameter <- funcParam
                            rightFirst.Expr <- FsExpr.Sequential(assignExpr, rightFirst.Expr) |> excast

                            connectMutNode (leftLast, rightFirst)
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
                                            <@ 0, Some %(waitCondExpr bodyExpr) @>)
                                    e, true
                            let rightNode = newNode (funcParam, nodeExpr, returnsWaitCondition)
                            connectMutNode (leftLast, rightNode)
                            Node (leftLast, rightNode)
                    else
                        let rightFirst, rightLast = createCfg (body, kind)
                        connectMutNode (leftLast, rightFirst)
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

                    let createWithTestNode () =
                        let testFirst, testLast = condToNode cond
                        testFirst.HasMultipleIncomingEdges <- true
                        let lastNode = doNothingNode ()
                        let (bodyFirst, bodyLast) = createCfg (arg2, Unit)

                        connectMutNode (testLast, lastNode)
                        connectMutNode (testLast, bodyFirst)
                        connectMutNode (bodyLast, testFirst)

                        Node (testFirst, lastNode)

                    match cond with
                    | Patterns.ValueWithName _ ->
                        // ValueWithName は定数なので DerivedPatterns.Bool にマッチするが
                        // 定数ではなく変数として扱いたいので、先に引っ掛ける。
                        createWithTestNode ()
                    | DerivedPatterns.Bool true ->
                        // 無限ループ
                        let (bodyFirst, bodyLast) = createCfg (arg2, Unit)
                        bodyFirst.HasMultipleIncomingEdges <- true
                        connectMutNode (bodyLast, bodyFirst)
                        Node (bodyFirst, bodyLast)
                    | DerivedPatterns.Bool false -> Expr <@ () @>
                    | _ -> createWithTestNode ()

                | _ -> failwith "The first argument of the While call is not a lambda."

            | DerivedPatterns.SpecificCall <@@ this.Zero @@> _ ->
                Expr <@ () @>

            | Patterns.Call (Some (Patterns.Value (receiver, _)), method, _)
                when obj.ReferenceEquals(receiver, this) ->
                failwithf "Do not call '%s'." method.Name

            // その他の文法

            | Patterns.IfThenElse (cond, trueExpr, falseExpr) ->
                let testFirst, testLast = condToNode cond

                let (trueFirst, trueLast), (falseFirst, falseLast), joinNode =
                    match kind with
                    | Unit | Assign _ ->
                        // true, false それぞれで、やることをやって、最後に何もしないノードに戻ってくる
                        let joinNode = doNothingNode ()
                        joinNode.HasMultipleIncomingEdges <- true
                        createCfg (trueExpr, kind), createCfg (falseExpr, kind), joinNode

                    | WaitCondition ->
                        let trueType, falseType = trueExpr.Type, falseExpr.Type
                        if not (obj.Equals(trueType, falseType)) then
                            failwithf "Type mismatch (True: %O, False: %O)" trueType falseType
                        ensureWaitConditionType trueType

                        // trueExpr, falseExpr が返す WaitCondition を変数に保存して、 joinNode でそれを返す
                        // TODO: WaitCondition を状態保存に含めるわけにはいかないので、うまく変数を使うのを回避する
                        let waitCondVar = tmpVar ("waitCondition", trueType)
                        addVar waitCondVar
                        let trueTuple = createCfg (trueExpr, Assign waitCondVar)
                        let falseTuple = createCfg (falseExpr, Assign waitCondVar)
                        let joinNode =
                            let varExpr = waitCondExpr (FsExpr.Var(waitCondVar))
                            let body = <@ 0, Some %varExpr @>
                            newNode (discardObjVar(), body, true)
                        joinNode.HasMultipleIncomingEdges <- true
                        trueTuple, falseTuple, joinNode

                connectMutNode (testLast, falseFirst)
                connectMutNode (falseLast, joinNode)
                connectMutNode (testLast, trueFirst)
                connectMutNode (trueLast, joinNode)
                Node (testFirst, joinNode)

            | Patterns.Let (var, value, body) -> transLet ([(var, value)], body, kind)
            | Patterns.LetRecursive (bindings, body) -> transLet (bindings, body, kind)

            | Patterns.Sequential (left, right) -> transCombine (left, right, kind)

            | Patterns.ValueWithName x ->
                let var = recordCapturedVar x
                Expr (FsExpr.Var(var.FsVar))

            | Patterns.VarSet (var, expr) ->
                match createCfgOrExpr (expr, Assign var) with
                | Expr x -> Expr (FsExpr.VarSet(var, x))
                | x -> x

            | Patterns.TryFinally _ -> raise (NotSupportedException("TryFinally"))
            | Patterns.TryWith _ -> raise (NotSupportedException("TryWith"))
            | Patterns.WhileLoop _ -> raise (NotSupportedException("WhileLoop"))

            | ExprShape.ShapeVar _ as expr -> Expr expr

            | ExprShape.ShapeLambda (_, body) as expr ->
                // ラムダ式内で参照された変数はエスケープとしてマーク
                let rec traverse = function
                    | Patterns.ValueWithName x ->
                        let var = recordCapturedVar x
                        var.IsEscaped <- true
                    | ExprShape.ShapeVar var ->
                        markAsEscaped var
                    | ExprShape.ShapeLambda (_, x) ->
                        traverse x
                    | ExprShape.ShapeCombination (_, exprs) ->
                        List.iter traverse exprs
                traverse body
                Expr expr

            | ExprShape.ShapeCombination (shape, args) ->
                let blocks = List()
                let currentBlock = List<FsExpr>()
                for argExpr in args do
                    let lastArgVar = tmpVar ("combArg", argExpr.Type)
                    match createCfgOrExpr (argExpr, Assign lastArgVar) with
                    | Node (nodeFirst, nodeLast) ->
                        match nodeLast.Expr with
                        | OneWayBody _ -> ()
                        | _ -> failwith "nodeLast.Expr is not a OneWayBody."

                        let otherExprs = currentBlock.ToArray()
                        currentBlock.Clear()

                        let otherArgsVar, nodeFirst =
                            match otherExprs.Length with
                            | 0 -> None, nodeFirst
                            | _ ->
                                let argTypes = otherExprs |> Seq.map (fun x -> x.Type) |> Seq.toArray
                                let otherArgsVar = tmpVar ("combArgs", FSharpType.MakeTupleType(argTypes))
                                addVar otherArgsVar

                                // nodeFirst の前に代入を挿入する
                                let assignExpr = FsExpr.VarSet(otherArgsVar, FsExpr.NewTuple(List.ofArray otherExprs))
                                Some otherArgsVar, tryPrependExpr assignExpr nodeFirst

                        addVar lastArgVar
                        blocks.Add(((nodeFirst, nodeLast), otherArgsVar, otherExprs.Length, lastArgVar))
                    | Expr x -> currentBlock.Add(x)

                if blocks.Count = 0 then
                    // CFG ノードへの変形は必要ないので、そのまま再構成する
                    Expr (ExprShape.RebuildShapeCombination(shape, List.ofSeq currentBlock))
                else
                    // CFG ノードを連結する
                    let assignNodeFirst, assignNodeLast =
                        let reduction (currentFirst, currentLast) (nextFirst, nextLast) =
                            connectMutNode (currentLast, nextFirst)
                            currentFirst, nextLast
                        blocks |> Seq.map (fun (x, _, _, _) -> x)
                               |> Seq.reduce reduction

                    // 代入された値を使って、式を組み立て直す
                    let rebuiltExpr =
                        let rebuiltArgs =
                            let blockToExprs (_, otherArgsVar, otherArgCount, lastArgVar) =
                                let exprs =
                                    match otherArgsVar with
                                    | Some x -> Seq.init otherArgCount (fun i -> FsExpr.TupleGet(FsExpr.Var(x), i))
                                    | None -> Seq.empty
                                Seq.append exprs (Seq.singleton (FsExpr.Var(lastArgVar)))
                            Seq.append (Seq.collect blockToExprs blocks) currentBlock
                        ExprShape.RebuildShapeCombination(shape, List.ofSeq rebuiltArgs)

                    // 最後のノードに式を追加する
                    let nodeExpr, returnsWaitCondition =
                        match assignNodeLast.Expr with
                        | OneWayBody body ->
                            let newBody =
                                match body with
                                | Some x -> FsExpr.Sequential(x, rebuiltExpr)
                                | None -> rebuiltExpr
                            match kind with
                            | Unit ->
                                FsExpr.Sequential(newBody, oneWay), false
                            | Assign var ->
                                FsExpr.Sequential(FsExpr.VarSet(var, newBody), oneWay), false
                            | WaitCondition ->
                                <@@ 0, Some %(waitCondExpr newBody) @@>, true
                        | _ -> failwith "assignNodeLast.Expr is not a OneWayBody."

                    assignNodeLast.Expr <- nodeExpr |> excast
                    assignNodeLast.ReturnsWaitCondition <- returnsWaitCondition
                    Node (assignNodeFirst, assignNodeLast)

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

                | [] -> invalidArg (nameof bindings) "bindings is empty."

            connectNodeOrExpr (bindingToNode bindings, createCfgOrExpr (body, kind))

        and transCombine (left, right, kind) =
            connectNodeOrExpr (createCfgOrExpr (left, Unit), createCfgOrExpr (right, kind))

        /// 条件を評価し、 false なら 0 番目、 true なら 1 番目の辺に進むノードを作成する
        and condToNode cond =
            let condVar = tmpVar ("cond", typeof<bool>)
            let condVarExpr = FsExpr.Cast<bool>(FsExpr.Var(condVar))

            let firstNode, lastNode = createCfg (cond, Assign condVar)
            let lastNode =
                match lastNode.Expr with
                | OneWayBody (None | Some DerivedPatterns.Unit) ->
                    // body がないので、 condVar によって分岐する式に完全に置き換える
                    addVar condVar
                    lastNode.Expr <- <@ (if %condVarExpr then 1 else 0), Option<WaitCondition>.None @>
                    lastNode
                | OneWayBody (Some body) ->
                    let proc, lastExpr = splitLastExpr body
                    match lastExpr with
                    | Patterns.VarSet (setVar, setExpr) when setVar = condVar ->
                        // 最後の式が condVar への代入ならば、代入ごと消す
                        let returnExpr = <@@ (if %%setExpr then 1 else 0), Option<WaitCondition>.None @@>
                        let newBody =
                            match proc with
                            | Some x -> FsExpr.Sequential(x, returnExpr)
                            | None -> returnExpr
                        lastNode.Expr <- newBody |> excast
                    | _ ->
                        // condVar によって分岐する
                        addVar condVar
                        let newBody =
                            FsExpr.Sequential(body,
                                <@@ (if %condVarExpr then 1 else 0), Option<WaitCondition>.None @@>)
                        lastNode.Expr <- newBody |> excast                            
                    lastNode
                | _ ->
                    addVar condVar
                    let newBody = <@ (if %condVarExpr then 1 else 0), Option<WaitCondition>.None @>
                    let condNode = newNode (discardObjVar(), newBody, false)
                    connectMutNode (lastNode, condNode)
                    condNode

            firstNode, lastNode

        /// 分岐しないノードをまとめる
        let reduceCfg rootNode =
            let nodes = enumerateNodes rootNode
            let incomingEdges =
                nodes
                |> mapToList (fun x ->
                    nodes |> Seq.filter (fun y -> y.Edges.Contains(x))
                          |> HashSet)
            // Convert to a mutable list
            let nodes = mapToList Some nodes

            #if DEBUG
            // HasMultipleIncomingEdges がバグってないかチェック
            for i = 0 to nodes.Count - 1 do
                match nodes.[i], incomingEdges.[i] with
                | Some node, edges ->
                    // ルートノードが while 文の場合は回避できないので無視
                    if node <> rootNode && node.HasMultipleIncomingEdges <> (edges.Count > 1) then
                        failwithf "HasMultipleIncomingEdges = %b, Count = %d"
                            node.HasMultipleIncomingEdges
                            edges.Count
                | _ -> failwith "unreachable"
            #endif

            // ループしながら nodes を書き換えるので、インデックス操作でやっていく
            for i = 0 to nodes.Count - 1 do
                match nodes.[i] with
                | Some node when node.Edges.Count = 1 ->
                    if not node.ReturnsWaitCondition then
                        match node.Expr with
                        | OneWayBody (None | Some DerivedPatterns.Unit) ->
                            // 何もしないノードなので、スキップできる
                            let nextNode = node.Edges.[0]
                            let nextNodeIncomingEdges = incomingEdges.[nodes.IndexOf(Some nextNode)]

                            for incomingNode in incomingEdges.[i] do
                                // Edges から node への辺を削除し、 nextNode への辺を追加する
                                incomingNode.Edges.[incomingNode.Edges.IndexOf(node)] <- nextNode
                                if not (nextNodeIncomingEdges.Add(incomingNode)) then
                                    failwith "Duplicate edge"

                            nextNode.HasMultipleIncomingEdges <- nextNodeIncomingEdges.Count > 1
                            nodes.[i] <- None
                            incomingEdges.[i].Clear()

                        | OneWayBody (Some body) when not node.Edges.[0].HasMultipleIncomingEdges ->
                            // 次のノードと連結できる
                            let nextNode = node.Edges.[0]
                            let nextNodeIndex = nodes.IndexOf(Some nextNode)

                            if refToParam nextNode then
                                failwith "nextBody refers to the lambda argument."

                            // ノードの書き換え
                            node.Expr <- FsExpr.Sequential(body, nextNode.Expr) |> excast
                            node.ReturnsWaitCondition <- nextNode.ReturnsWaitCondition

                            node.Edges.Clear()
                            node.Edges.AddRange(nextNode.Edges)

                            nodes.[nextNodeIndex] <- None
                            incomingEdges.[nextNodeIndex].Clear()

                            // incomingEdges の更新
                            for incomingSet in incomingEdges do
                                if incomingSet.Remove(nextNode) then
                                    if not (incomingSet.Add(node)) then
                                        failwith "Duplicate edge"

                        | OneWayBody _ -> ()
                        | _ -> failwith "node.Expr is not a OneWayBody."

                | _ -> ()

        let rootNode, _ = createCfg (expr, Unit)
        reduceCfg rootNode

        let vars =
            let toImmutableVar x : ImmutableVar =
                { FsVar = x.FsVar
                  CapturedValue = x.CapturedValue
                  IsEscaped = x.IsEscaped }
            Seq.map toImmutableVar env.Variables.Values

        let graph = createImmutableGraph (vars, rootNode)
        ProcessModelBuilderResult<'I>(graph)
