module PopDEVS.Tests.ProcessOriented.StateReducerTests

open System.Collections.Generic
open System.Collections.Immutable
open Expecto
open FSharp.Quotations
open PopDEVS.ProcessOriented
open MutableCfg

let private lambdaToNode (expr: FsExpr<obj -> int * WaitCondition option>) =
    match expr with
    | Patterns.Lambda (lambdaVar, lambdaBody) ->
        let returnsWaitCondition =
            match lambdaBody with
            | ReturnsWaitCondition -> true
            | _ -> false
        { LambdaParameter = lambdaVar
          Expr = lambdaBody |> excast
          HasMultipleIncomingEdges = false
          ReturnsWaitCondition = returnsWaitCondition
          Edges = List() }
    | _ -> invalidArg (nameof expr) "expr is not a lambda expression."

let private oneWayLambda = <@ fun _ -> 0, None @>

let private reduceIfTests =
    testList "reduceIf" [
        test "merge node is branch" {
            let condNode = lambdaToNode <@ fun _ -> ignore "cond"; (if true then 1 else 0), None @>
            let leftNode = lambdaToNode <@ fun _ -> ignore "when false"; 0, None @>
            let rightNode = lambdaToNode <@ fun _ -> ignore "when true"; 0, None @>
            let mergeNode = lambdaToNode <@ fun _ -> ignore "merge"; (if false then 1 else 0), None @>
            mergeNode.HasMultipleIncomingEdges <- true
            let nextNode1 = lambdaToNode oneWayLambda
            let nextNode2 = lambdaToNode oneWayLambda

            connectMutNode(condNode, leftNode)
            connectMutNode(condNode, rightNode)
            connectMutNode(leftNode, mergeNode)
            connectMutNode(rightNode, mergeNode)
            connectMutNode(mergeNode, nextNode1)
            connectMutNode(mergeNode, nextNode2)

            let incomingEdges = StateReducer.makeIncomingDic condNode
            Expect.hasLength incomingEdges 6 "initially there are 6 nodes"

            let env = { StateReducer.IncomingEdges = incomingEdges }
            StateReducer.reduceIf env (condNode, leftNode, rightNode, mergeNode)

            Expect.hasLength incomingEdges 3 "leftNode, rightNode and mergeNode are removed"
            Expect.isEmpty incomingEdges.[condNode] "condNode has no incoming edge"
            Expect.isTrue (incomingEdges.[nextNode1].SetEquals([condNode])) "nextNode1 has an incoming edge from condNode"
            Expect.isTrue (incomingEdges.[nextNode2].SetEquals([condNode])) "nextNode2 has an incoming edge from condNode"

            let expectedExpr =
                <@ ignore "cond";
                   if true then ignore "when true" else ignore "when false"
                   ignore "merge"
                   (if false then 1 else 0), None @>
            Expect.equal condNode.Expr expectedExpr "condNode.Expr is transformed"

            Expect.isFalse condNode.HasMultipleIncomingEdges "HasMultipleIncomingEdges is not changed"
            Expect.isFalse condNode.ReturnsWaitCondition "condNode.ReturnsWaitCondition <- mergeNode.ReturnsWaitCondition"
            Expect.sequenceEqual condNode.Edges [nextNode1; nextNode2] "condNode has outgoing edges to nextNode1 and nextNode2"
        }

        test "merge node returns WaitCondition" {
            let waitCond = Unchecked.defaultof<WaitCondition<int, unit>>
            let condNode = lambdaToNode <@ fun _ -> ignore "cond"; (if true then 1 else 0), None @>
            let leftNode = lambdaToNode <@ fun _ -> ignore "when false"; 0, None @>
            let rightNode = lambdaToNode <@ fun _ -> ignore "when true"; 0, None @>
            let mergeNode = lambdaToNode <@ fun _ -> ignore "merge"; 0, Some (waitCond :> WaitCondition) @>
            mergeNode.HasMultipleIncomingEdges <- true
            let nextNode = lambdaToNode oneWayLambda

            connectMutNode(condNode, leftNode)
            connectMutNode(condNode, rightNode)
            connectMutNode(leftNode, mergeNode)
            connectMutNode(rightNode, mergeNode)
            connectMutNode(mergeNode, nextNode)

            Expect.isFalse condNode.ReturnsWaitCondition "condNode does not return WaitCondition before reducing"

            let incomingEdges = StateReducer.makeIncomingDic condNode
            Expect.hasLength incomingEdges 5 "initially there are 5 nodes"

            let env = { StateReducer.IncomingEdges = incomingEdges }
            StateReducer.reduceIf env (condNode, leftNode, rightNode, mergeNode)

            Expect.hasLength incomingEdges 2 "leftNode, rightNode and mergeNode are removed"
            Expect.isEmpty incomingEdges.[condNode] "condNode has no incoming edge"
            Expect.isTrue (incomingEdges.[nextNode].SetEquals([condNode])) "nextNode has an incoming edge from condNode"

            let expectedExpr =
                <@ ignore "cond";
                   if true then ignore "when true" else ignore "when false"
                   ignore "merge"
                   0, Some (waitCond :> WaitCondition) @>
            Expect.equal condNode.Expr expectedExpr "condNode.Expr is transformed"

            Expect.isFalse condNode.HasMultipleIncomingEdges "HasMultipleIncomingEdges is not changed"
            Expect.isTrue condNode.ReturnsWaitCondition "condNode.ReturnsWaitCondition <- mergeNode.ReturnsWaitCondition"
            Expect.sequenceEqual condNode.Edges [nextNode] "condNode has an outgoing edge to nextNode"
        }
    ]

let private reduceWhileTests =
    testList "reduceWhile" [
        test "exit node is branch" {
            let prevNode = lambdaToNode oneWayLambda
            let condNode = lambdaToNode <@ fun _ -> ignore "cond"; (if true then 1 else 0), None @>
            condNode.HasMultipleIncomingEdges <- true
            let loopBodyNode = lambdaToNode <@ fun _ -> ignore "loop"; 0, None @>
            let exitNode = lambdaToNode <@ fun _ -> ignore "exit"; (if false then 1 else 0), None @>
            let nextNode1 = lambdaToNode oneWayLambda
            let nextNode2 = lambdaToNode oneWayLambda

            connectMutNode(prevNode, condNode)
            connectMutNode(condNode, exitNode)
            connectMutNode(condNode, loopBodyNode)
            connectMutNode(loopBodyNode, condNode)
            connectMutNode(exitNode, nextNode1)
            connectMutNode(exitNode, nextNode2)

            let incomingEdges = StateReducer.makeIncomingDic prevNode
            Expect.hasLength incomingEdges 6 "initially there are 6 nodes"

            let env = { StateReducer.IncomingEdges = incomingEdges }
            StateReducer.reduceWhile env (condNode, loopBodyNode, exitNode)

            Expect.hasLength incomingEdges 4 "loopBody and exitNode are removed"
            Expect.isEmpty incomingEdges.[prevNode] "prevNode has no incoming edge"
            Expect.isTrue (incomingEdges.[condNode].SetEquals([prevNode])) "condNode has an incoming edge from prevNode"
            Expect.isTrue (incomingEdges.[nextNode1].SetEquals([condNode])) "nextNode1 has an incoming edge from condNode"
            Expect.isTrue (incomingEdges.[nextNode2].SetEquals([condNode])) "nextNode2 has an incoming edge from condNode"

            let expectedExpr =
                <@ ignore "cond"
                   while true do ignore "loop"
                   ignore "exit"
                   (if false then 1 else 0), None @>
            Expect.equal condNode.Expr expectedExpr "condNode.Expr is transformed"

            Expect.isFalse condNode.HasMultipleIncomingEdges "the edge from loopBodyNode to condNode is removed"
            Expect.isFalse condNode.ReturnsWaitCondition "condNode.ReturnsWaitCondition <- exitNode.ReturnsWaitCondition"
            Expect.sequenceEqual condNode.Edges [nextNode1; nextNode2] "condNode has an outgoing edge to nextNode"
        }

        test "exit node returns WaitCondition" {
            let waitCond = Unchecked.defaultof<WaitCondition<int, unit>>
            let prevNode = lambdaToNode oneWayLambda
            let condNode = lambdaToNode <@ fun _ -> ignore "cond"; (if true then 1 else 0), None @>
            condNode.HasMultipleIncomingEdges <- true
            let loopBodyNode = lambdaToNode <@ fun _ -> ignore "loop"; 0, None @>
            let exitNode = lambdaToNode <@ fun _ -> ignore "exit"; 0, Some (waitCond :> WaitCondition) @>
            let nextNode = lambdaToNode oneWayLambda

            connectMutNode(prevNode, condNode)
            connectMutNode(condNode, exitNode)
            connectMutNode(condNode, loopBodyNode)
            connectMutNode(loopBodyNode, condNode)
            connectMutNode(exitNode, nextNode)

            Expect.isTrue condNode.HasMultipleIncomingEdges "condNode has multiple incoming edges before reducing"
            Expect.isFalse condNode.ReturnsWaitCondition "condNode does not return WaitCondition before reducing"

            let incomingEdges = StateReducer.makeIncomingDic prevNode
            Expect.hasLength incomingEdges 5 "initially there are 5 nodes"

            let env = { StateReducer.IncomingEdges = incomingEdges }
            StateReducer.reduceWhile env (condNode, loopBodyNode, exitNode)

            Expect.hasLength incomingEdges 3 "loopBody and exitNode are removed"
            Expect.isEmpty incomingEdges.[prevNode] "prevNode has no incoming edge"
            Expect.isTrue (incomingEdges.[condNode].SetEquals([prevNode])) "condNode has an incoming edge from prevNode"
            Expect.isTrue (incomingEdges.[nextNode].SetEquals([condNode])) "nextNode has an incoming edge from condNode"

            let expectedExpr =
                <@ ignore "cond";
                   while true do ignore "loop"
                   ignore "exit"
                   0, Some (waitCond :> WaitCondition) @>
            Expect.equal condNode.Expr expectedExpr "condNode.Expr is transformed"

            Expect.isFalse condNode.HasMultipleIncomingEdges "the edge from loopBodyNode to condNode is removed"
            Expect.isTrue condNode.ReturnsWaitCondition "condNode.ReturnsWaitCondition <- exitNode.ReturnsWaitCondition"
            Expect.sequenceEqual condNode.Edges [nextNode] "condNode has an outgoing edge to nextNode"
        }
    ]

let private reduceExitNodesTests =
    let removeConditionEdgeTest removableNodeReturnsWaitCondition () =
        let waitCond = Unchecked.defaultof<WaitCondition<int, unit>>
        let condNodeExpr = <@ (if true then 1 else 0), None @>
        let condNode = lambdaToNode <@ fun _ -> %condNodeExpr @>
        let removableNodeWaitCondExpr =
            if removableNodeReturnsWaitCondition
            then <@ Some (waitCond :> WaitCondition) @> else <@ None @>
        let removableNodeExpr = <@ ignore "removable"; 0, %removableNodeWaitCondExpr @>
        let removableNode = lambdaToNode <@ fun _ -> %removableNodeExpr @>
        let trueNode = lambdaToNode <@ fun _ -> 0, Some (waitCond :> WaitCondition) @>
        let notRemovableNode = lambdaToNode oneWayLambda

        connectMutNode(condNode, removableNode)
        connectMutNode(condNode, trueNode)
        connectMutNode(trueNode, notRemovableNode)

        Expect.equal
            removableNode.ReturnsWaitCondition removableNodeReturnsWaitCondition
            "removableNode.ReturnsWaitCondition equals to the test parameter"

        StateReducer.reduceExitNodes condNode

        Expect.sequenceEqual
            (enumerateNodes condNode)
            [condNode; trueNode; notRemovableNode]
            "removableNode is removed"

        Expect.isFalse condNode.HasMultipleIncomingEdges "condNode has no incoming edge"
        Expect.isFalse condNode.ReturnsWaitCondition "condNode does not return WaitCondition if the edge index is 0"
        Expect.sequenceEqual condNode.Edges [trueNode] "the edge from condNode to removableNode is removed"

        match condNode.Expr with
        | Patterns.Let (retTupleVar, letExpr, body) ->
            let retTupleExpr = FsExpr.Var(retTupleVar) |> FsExpr.Cast<int * WaitCondition option>
            Expect.equal letExpr condNodeExpr.Raw "let retTuple = (if true then 1 else 0), None"

            match body with
            | Patterns.Let (edgeIndexVar, letExpr, body) ->
                Expect.equal letExpr
                    (FsExpr.TupleGet(FsExpr.Var(retTupleVar), 0))
                    "let edgeIndex = fst retTuple"

                match body with
                | Patterns.IfThenElse(eqCond, exprIfEq, Patterns.IfThenElse(gtCond, exprIfGt, exprIfLt)) ->
                    let edgeIndexExpr = FsExpr.Var(edgeIndexVar) |> FsExpr.Cast<int>

                    Expect.equal eqCond <@@ %edgeIndexExpr = 0 @@> "if edgeIndex = 0"

                    Expect.equal exprIfEq
                        <@@ ignore "removable"; -1, %removableNodeWaitCondExpr @@>
                        ("then ignore \"removable\"; -1, " + (if removableNodeReturnsWaitCondition then "Some waitCond" else "None"))

                    Expect.equal gtCond <@@ %edgeIndexExpr > 0 @@> "elif edgeIndex > 0"

                    Expect.equal exprIfGt
                        <@@ %edgeIndexExpr - 1, %(FsExpr.TupleGet(retTupleExpr, 1) |> FsExpr.Cast<WaitCondition option>) @@>
                        "then edgeIndex -1, snd retTuple"

                    Expect.equal exprIfLt retTupleExpr.Raw "else retTuple"

                | x -> failtestf "The body of the let expression for 'edgeIndex' is not if-elif-else. Actual: %A" x
            | x -> failtestf "The body of the let expression for 'retTuple' is not let. Actual: %A" x
        | x -> failtestf "The root of condNode.Expr is not let. Actual: %A" x

    testList "reduceExitNodes" [
        testCase "remove condition edge" <| removeConditionEdgeTest false
        testCase "remove condition edge returning WaitCondition" <| removeConditionEdgeTest true

        test "don't remove node if the node has 2 incoming edges" {
            let condLambda = <@ fun _ -> (if true then 1 else 0), Option<WaitCondition>.None @>
            let condNode1 = lambdaToNode condLambda
            let notRemovableNode1 = lambdaToNode oneWayLambda
            notRemovableNode1.HasMultipleIncomingEdges <- true
            let condNode2 = lambdaToNode condLambda
            let waitNode =
                let waitCond = Unchecked.defaultof<WaitCondition<int, unit>>
                lambdaToNode <@ fun _ -> 0, Some (waitCond :> WaitCondition) @>
            let notRemovableNode2 = lambdaToNode oneWayLambda

            connectMutNode(condNode1, notRemovableNode1)
            connectMutNode(condNode1, condNode2)
            connectMutNode(condNode2, notRemovableNode1)
            connectMutNode(condNode2, waitNode)
            connectMutNode(waitNode, notRemovableNode2)

            StateReducer.reduceExitNodes condNode1

            Expect.hasLength (enumerateNodes condNode1) 5 "no node is removed"
        }
    ]

let private reduceGraphTests =
    let variableSet (graph: ImmutableGraph) =
        graph.Variables
        |> Seq.map (fun x -> x.FsVar.Name, x.FsVar.Type, x.CapturedValue, x.IsEscaped)
        |> ImmutableHashSet.CreateRange

    testList "reduceGraph" [
        test "example 1" {
            let returnInputWaitCondition = Unchecked.defaultof<WaitCondition<int, int>>
            let unitWaitCondition = Unchecked.defaultof<WaitCondition<int, unit>>

            let builderResult =
                processModel {
                    let mutable i = 1
                    while i <= 9 do
                        let! v = returnInputWaitCondition
                        if v % 2 = 0 then
                            do! unitWaitCondition
                        i <- i + 1
                }

            let graph = builderResult.ControlFlowGraph
            Expect.hasLength graph.Nodes 7 "graph has 7 nodes"

            let expectedVars = [
                "i", typeof<int>, None, false
                "v", typeof<int>, None, false
                (nameof returnInputWaitCondition), typeof<WaitCondition<int, int>>, Some (box returnInputWaitCondition), false
                (nameof unitWaitCondition), typeof<WaitCondition<int, unit>>, Some (box unitWaitCondition), false
            ]
            Expect.isTrue ((variableSet graph).SetEquals(expectedVars)) "variables are i and v"

            let reducedGraph = StateReducer.reduceGraph graph
            Expect.hasLength reducedGraph.Nodes 5 "2 nodes are removed"

            let expectedVars = [
                "i", typeof<int>, None, false
                (nameof returnInputWaitCondition), typeof<WaitCondition<int, int>>, Some (box returnInputWaitCondition), false
                (nameof unitWaitCondition), typeof<WaitCondition<int, unit>>, Some (box unitWaitCondition), false
            ]
            Expect.isTrue ((variableSet reducedGraph).SetEquals(expectedVars)) "v is removed"
        }

        test "example 2" {
            let boolWaitCond = Unchecked.defaultof<WaitCondition<int, bool>>
            let unitWaitCond = Unchecked.defaultof<WaitCondition<int, unit>>
            let firstCond = true
            let thirdCond = false

            let builderResult =
                processModel {
                    let! secondCond =
                        if firstCond then
                            let msg1 = "firstCond is true"
                            ignore msg1
                            boolWaitCond
                        else
                            let msg2 = "firstCond is false"
                            ignore msg2
                            boolWaitCond

                    if secondCond then
                        ignore "secondCond is true"

                        if thirdCond then
                            ignore "thirdCond is true"

                            let mutable i = 1
                            while i <= 9 do
                                ignore "loop body"
                                do! unitWaitCond

                            ignore "loop exit"
                        else
                            ignore "thirdCond is false"
                            do! unitWaitCond
                    else
                        ignore "secondCond is false"
                        do! unitWaitCond
                }

            let graph = builderResult.ControlFlowGraph
            Expect.hasLength graph.Nodes 13 "initial graph has 13 nodes"

            let reducedGraph = StateReducer.reduceGraph graph
            ignore ()
            // Expect.hasLength reducedGraph.Nodes 4 "reduced graph has 4 nodes"
        }
    ]

[<Tests>]
let tests =
    testList "StateReducer" [
        reduceIfTests
        reduceWhileTests
        reduceExitNodesTests
        reduceGraphTests
    ]
