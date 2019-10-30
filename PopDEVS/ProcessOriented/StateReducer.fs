module internal PopDEVS.ProcessOriented.StateReducer

open System
open System.Collections.Generic
open System.Linq
open FSharp.Quotations
open MutableCfg
open PopDEVS

type ReduceEnv =
    { IncomingEdges: Dictionary<MutableNode, HashSet<MutableNode>> }

    member this.RemoveNodes(nodes) =
        for x in nodes do
            if not (this.IncomingEdges.Remove(x)) then
                failwith "The node has already been removed."

let private replaceEdges env (oldNode, newNode) =
    newNode.Edges.Clear()
    newNode.Edges.AddRange(oldNode.Edges)
                
    for dst in newNode.Edges do
        let dstIncomingEdges = env.IncomingEdges.[dst]
        if not (dstIncomingEdges.Remove(oldNode)) then
            failwith "The edge has already been removed."
        if not (dstIncomingEdges.Add(newNode)) then
            failwith "Duplicate edge"

let private toUnitExpr (expr: FsExpr<int * obj option>) =
    match expr with
    | OneWayBody (Some body) ->
        let newBody =
            if obj.Equals(body.Type, typeof<unit>) then
                body
            else
                FsExpr.Sequential(body, <@@ () @@>)
        FsExpr.Cast(newBody)
    | OneWayBody None -> <@ () @>
    | _ -> invalidArg (nameof expr) "expr is not OneWayBody"

/// ノードの入力辺を求める
let makeIncomingDic rootNode =
    let dic = Dictionary()
    let rec traverse node =
        if not (dic.ContainsKey(node)) then
            dic.Add(node, HashSet())
            node.Edges |> Seq.iter traverse
    traverse rootNode

    for node in dic.Keys do
        for edge in node.Edges do
            dic.[edge].Add(node) |> ignore

    dic

let reduceIf (env: ReduceEnv) (cond, left, right, merge) =
    let newCondBody =
        let condBody, condLast = splitLastExpr cond.Expr
        match condLast with
        | Patterns.NewTuple [Patterns.IfThenElse (condExpr, DerivedPatterns.Int32 1, DerivedPatterns.Int32 0); waitCondOptionExpr]
          when waitCondOptionExpr = <@@ None @@> ->
            let expr =
                FsExpr.Sequential(
                    FsExpr.IfThenElse(condExpr,
                                      toUnitExpr right.Expr,
                                      toUnitExpr left.Expr),
                    merge.Expr)
            match condBody with
            | Some x -> FsExpr.Sequential(x, expr)
            | None -> expr
        | _ -> failwith "cond.Expr has not a well-formed IfThenElse expression."

    cond.Expr <- newCondBody |> excast
    cond.ReturnsWaitCondition <- merge.ReturnsWaitCondition

    env.RemoveNodes([| left; right; merge |])
    replaceEdges env (merge, cond)

let reduceWhile (env: ReduceEnv) (cond, loopBody, exit) =
    let newCondBody =
        let condBody, condLast = splitLastExpr cond.Expr
        match condLast with
        | Patterns.NewTuple [Patterns.IfThenElse (condExpr, DerivedPatterns.Int32 1, DerivedPatterns.Int32 0); waitCondOptionExpr]
          when waitCondOptionExpr = <@@ None @@> ->
            let expr =
                FsExpr.Sequential(
                    FsExpr.WhileLoop(condExpr, toUnitExpr loopBody.Expr),
                    exit.Expr)
            match condBody with
            | Some x -> FsExpr.Sequential(x, expr)
            | None -> expr
        | _ -> failwith "cond.Expr has not a well-formed IfThenElse expression."

    cond.Expr <- newCondBody |> excast
    cond.ReturnsWaitCondition <- exit.ReturnsWaitCondition

    env.RemoveNodes([| loopBody; exit |])
    replaceEdges env (exit, cond)

    if not (env.IncomingEdges.[cond].Remove(loopBody)) then
        failwith "A edge from loopBody to cond has already been removed."
    cond.HasMultipleIncomingEdges <- env.IncomingEdges.[cond].Count >= 2

let tryReduceIfOrWhile env (startNode: MutableNode) =
    // reduceCfg によって簡略化されたグラフなので、2ホップ読めば安全にまとめられるか判断できる

    if startNode.Edges.Count <> 2 || startNode.ReturnsWaitCondition then
        // 分岐ではないので、何もできない
        false
    else
        let getNextNode node =
            if node.Edges.Count = 1 then Some node.Edges.[0] else None

        let nodesTuple =
            let left = startNode.Edges.[0]
            let right = startNode.Edges.[1]
            left, getNextNode left, right, getNextNode right

        let (|InterNode|_|) node =
            let rejectCond =
                node.ReturnsWaitCondition ||
                node.Edges.Count <> 1 ||
                env.IncomingEdges.[node].Count <> 1 ||
                refToParam node
            rejectCond |> not |> boolToOption

        let (|Incoming|_|) count node =
            env.IncomingEdges.[node].Count = count |> boolToOption

        match nodesTuple with
        | InterNode as left, Some (Incoming 2 as x), (InterNode as right), Some y when x = y ->
            // if pattern
            reduceIf env (startNode, left, right, x)
            true
        | (Incoming 1 as exitNode), _, (InterNode as right), Some x when x = startNode ->
            // while pattern
            reduceWhile env (startNode, right, exitNode)
            true
        | _ -> false

/// 待機を行わない if, while 文をすべて簡略化する
let rec reduceBranches rootNode =
    let env = { IncomingEdges = makeIncomingDic rootNode }
    let visitedNodes = HashSet()
    let mutable reduced = false

    let rec traverse node =
        if visitedNodes.Add(node) then
            if tryReduceIfOrWhile env node then
                reduced <- true
            node.Edges |> Seq.iter traverse
    traverse rootNode

    // 変化がなくなるまで繰り返す
    if reduced then reduceBranches rootNode

/// if から終端ノードへの遷移を省略する
let reduceExitNodes rootNode =
    let visitedNodes = HashSet()

    let isRemovableExitNode node =
        (not node.HasMultipleIncomingEdges) &&
        node.Edges.Count = 0 &&
        (not <| visitedNodes.Contains(node))

    /// 戻り値の辺インデックスを不正な値（-1）に書き換える
    let rewriteToReturnInvalidIndex expr =
        let body, last = splitLastExpr expr
        match last with
        | Patterns.NewTuple [DerivedPatterns.Int32 0; waitCondExpr] ->
            let newLast = FsExpr.NewTuple([FsExpr.Value(-1); waitCondExpr])
            match body with
            | Some x -> FsExpr.Sequential(x, newLast)
            | None -> newLast
        | _ -> failwith "node does not returns 0 as the index of edge."

    let removeExitNode (node, index) =
        let retTupleVar = FsVar("retTuple", typeof<int * obj option>, false)
        let retTupleExpr = FsExpr.Var(retTupleVar) |> FsExpr.Cast<int * obj option>
        let edgeIndexVar = FsVar("edgeIndex", typeof<int>, false)
        let edgeIndexExpr = FsExpr.Var(edgeIndexVar) |> FsExpr.Cast<int>
        let retTupleSnd = FsExpr.TupleGet(retTupleExpr, 1) |> FsExpr.Cast<obj option>
        let indexExpr = FsExpr.Value(index) |> FsExpr.Cast<int>
        let otherEdgeExpr =
            if index < node.Edges.Count - 1 then
                // 削除する辺が、最後の辺でないならば、返す辺のインデックスを書き換える必要がある
                <@ if %edgeIndexExpr > %indexExpr
                   then %edgeIndexExpr - 1, %retTupleSnd
                   else %retTupleExpr @>
            else
                retTupleExpr
        let exitNodeExpr = rewriteToReturnInvalidIndex node.Edges.[index].Expr
        node.Expr <-
            // let retTuple = (original expr)
            // let edgeIndex, _ = retTuple
            // if edgeIndex = index then
            //     (exit node expr)
            // elif edgeIndex > index then
            //     edgeIndex - 1, snd retTuple
            // else
            //     retTuple
            FsExpr.Let(
                retTupleVar,
                node.Expr,
                FsExpr.Let(
                    edgeIndexVar,
                    FsExpr.TupleGet(retTupleExpr, 0),
                    FsExpr.IfThenElse(
                        <@@ %edgeIndexExpr = %indexExpr @@>,
                        exitNodeExpr,
                        otherEdgeExpr)))
            |> excast
        node.Edges.RemoveAt(index)

    let rec traverse node =
        if visitedNodes.Add(node) then
            if not (node.ReturnsWaitCondition) then
                let rec tryReduce index =
                    if index < node.Edges.Count then
                        if isRemovableExitNode node.Edges.[index] then
                            removeExitNode (node, index)
                            tryReduce index
                        else
                            tryReduce (index + 1)
                tryReduce 0

            node.Edges |> Seq.iter traverse
    traverse rootNode

let combineOneWayNodes rootNode =
    // 終端ノードを消去したことで、結合可能になったノードが増えたかもしれない。
    // ProcessModelBuilderImpl.reduceCfg と同じ処理だが、式の形が
    // OneWayBody ではなくなっているので、一般的な方法で結合する。

    let visitedNodes = HashSet()
    let rec traverse node =
        if visitedNodes.Add(node) then
            if (not node.ReturnsWaitCondition) &&
                node.Edges.Count = 1 &&
                (not node.Edges.[0].HasMultipleIncomingEdges)
            then
                let nextNode = node.Edges.[0]
                let retTupleVar = FsVar("retTuple", typeof<int * obj option>, false)
                let retTupleExpr = FsExpr.Var(retTupleVar) |> FsExpr.Cast<int * obj option>
                let retTupleFst = FsExpr.TupleGet(retTupleExpr, 0) |> FsExpr.Cast<int>
                let retTupleSnd = FsExpr.TupleGet(retTupleExpr, 1) |> FsExpr.Cast<obj option>
                node.Expr <-
                    // let retTuple = (original expr)
                    // if fst retTuple = 0 then
                    //     (next node expr)
                    // else
                    //     -1, snd retTuple
                    FsExpr.Let(
                        retTupleVar,
                        node.Expr,
                        FsExpr.IfThenElse(
                            <@@ %retTupleFst = 0 @@>,
                            nextNode.Expr,
                            <@@ -1, %retTupleSnd @@>))
                    |> excast
                node.ReturnsWaitCondition <- nextNode.ReturnsWaitCondition
                node.Edges.Clear()
                node.Edges.AddRange(nextNode.Edges)

            node.Edges |> Seq.iter traverse
    traverse rootNode

let private defauleOfMethodInfo =
    match <@@ Unchecked.defaultof<obj> @@> with
    | Patterns.Call (_, methodInfo, _) ->
        methodInfo.GetGenericMethodDefinition()
    | _ -> failwith "unreachable"

let rewriteVarToLet (node, var: ImmutableVar) =
    node.Expr <-
        FsExpr.Let(var.FsVar,
            FsExpr.Call(defauleOfMethodInfo.MakeGenericMethod(var.FsVar.Type), []),
            node.Expr)
        |> excast

/// 1ノード内でしか使われていない変数を、 let に置き換える
let reduceVariables (vars: seq<ImmutableVar>, rootNode) =
    // 変数を参照しているノードを数える
    let varReferences = vars.ToDictionary((fun x -> x.FsVar), (fun x -> x, HashSet()))
    let visitedNode = HashSet()
    let rec traverseNode node =
        if visitedNode.Add(node) then
            node.Expr.GetFreeVars()
                |> Seq.except [node.LambdaParameter]
                |> Seq.iter (fun x -> (snd varReferences.[x]).Add(node) |> ignore)
            node.Edges |> Seq.iter traverseNode    
    traverseNode rootNode

    // 1ノードからしか参照されていない変数は書き換えられる
    let rewritableNodes =
        varReferences.Values |> Seq.choose (fun (var, refs) ->
            match var.CapturedValue, var.IsEscaped, Seq.tryExactlyOne refs with
            | None, false, Some node -> Some (node, var)
            | _ -> None)

    rewritableNodes |> Seq.iter rewriteVarToLet

    // 残った変数だけを返す
    varReferences.Values |> Seq.choose (fun (var, refs) ->
        if var.CapturedValue.IsSome || var.IsEscaped || refs.Count >= 2
        then Some var else None)

/// 制御フローグラフから、 Atomic モデルとして必要な状態のノードだけになるよう、ノードの結合を行います。
let reduceGraph (graph: ImmutableGraph) : ImmutableGraph =
    let rootNode = (createMutableNodes graph).[0]
    reduceBranches rootNode
    reduceExitNodes rootNode
    combineOneWayNodes rootNode
    let vars = reduceVariables(graph.Variables, rootNode)
    createImmutableGraph(vars, rootNode)
