module internal PopDEVS.ProcessOriented.StateReducer

open System.Collections.Generic
open FSharp.Quotations
open MutableCfg
open PopDEVS

type ReduceEnv =
    { IncomingEdges: Dictionary<MutableNode, HashSet<MutableNode>> }

    member this.RemoveNodes(nodes) =
        for x in nodes do
            if not (this.IncomingEdges.Remove(x)) then
                failwith "The node has already been removed."

let toUnitExpr (expr: FsExpr<int * obj option>) =
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

let replaceEdges env (oldNode, newNode) =
    newNode.Edges.Clear()
    newNode.Edges.AddRange(oldNode.Edges)

    for dst in newNode.Edges do
        let dstIncomingEdges = env.IncomingEdges.[dst]
        if not (dstIncomingEdges.Remove(oldNode)) then
            failwith "The edge has already been removed."
        if not (dstIncomingEdges.Add(newNode)) then
            failwith "Duplicate edge"

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
    let env =
        // 各ノードの入力を求める
        let incomingEdges = Dictionary()
        let rec traverse node =
            if not (incomingEdges.ContainsKey(node)) then
                incomingEdges.Add(node, HashSet())
                node.Edges |> Seq.iter traverse
        for node in incomingEdges.Keys do
            for edge in node.Edges do
                incomingEdges.[edge].Add(node) |> ignore
        { IncomingEdges = incomingEdges }

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

// TODO: if から終端ノードへの遷移を簡略化する
// TODO: 1ノード内でしか使われていない変数を、 let に置き換える

//let reduceToStateMachine rootNode =    

/// 制御フローグラフから、 Atomic モデルとして必要な状態のノードだけになるよう、ノードの結合を行います。
let reduceGraph (graph: ImmutableGraph) : ImmutableGraph =
    raise (System.NotImplementedException())
