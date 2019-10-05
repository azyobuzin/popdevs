module PopDEVS.Tests.ProcessOriented.StateReducerTests

open System.Collections.Generic
open Expecto
open FSharp.Quotations
open PopDEVS.ProcessOriented
open MutableCfg

let private lambdaToNode (expr: FsExpr<obj -> int * obj option>) =
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

[<Tests>]
let tests =
    testList "StateReducer" [
        test "reduceIf; the merge node is branch" {
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

            let incomingEdges = Dictionary()
            incomingEdges.Add(condNode, HashSet())
            incomingEdges.Add(leftNode, HashSet([condNode]))
            incomingEdges.Add(rightNode, HashSet([condNode]))
            incomingEdges.Add(mergeNode, HashSet([leftNode; rightNode]))
            incomingEdges.Add(nextNode1, HashSet([mergeNode]))
            incomingEdges.Add(nextNode2, HashSet([mergeNode]))

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

        test "reduceIf; the merge node returns WaitCondition" {
            let waitCond = Unchecked.defaultof<WaitCondition<int, unit>>
            let condNode = lambdaToNode <@ fun _ -> ignore "cond"; (if true then 1 else 0), None @>
            let leftNode = lambdaToNode <@ fun _ -> ignore "when false"; 0, None @>
            let rightNode = lambdaToNode <@ fun _ -> ignore "when true"; 0, None @>
            let mergeNode = lambdaToNode <@ fun _ -> ignore "merge"; 0, Some (waitCond :> obj) @>
            mergeNode.HasMultipleIncomingEdges <- true
            let nextNode = lambdaToNode oneWayLambda

            connectMutNode(condNode, leftNode)
            connectMutNode(condNode, rightNode)
            connectMutNode(leftNode, mergeNode)
            connectMutNode(rightNode, mergeNode)
            connectMutNode(mergeNode, nextNode)

            let incomingEdges = Dictionary()
            incomingEdges.Add(condNode, HashSet())
            incomingEdges.Add(leftNode, HashSet([condNode]))
            incomingEdges.Add(rightNode, HashSet([condNode]))
            incomingEdges.Add(mergeNode, HashSet([leftNode; rightNode]))
            incomingEdges.Add(nextNode, HashSet([mergeNode]))

            Expect.isFalse condNode.ReturnsWaitCondition "condNode does not return WaitCondition before reducing"

            let env = { StateReducer.IncomingEdges = incomingEdges }
            StateReducer.reduceIf env (condNode, leftNode, rightNode, mergeNode)

            Expect.hasLength incomingEdges 2 "leftNode, rightNode and mergeNode are removed"
            Expect.isEmpty incomingEdges.[condNode] "condNode has no incoming edge"
            Expect.isTrue (incomingEdges.[nextNode].SetEquals([condNode])) "nextNode has an incoming edge from condNode"

            let expectedExpr =
                <@ ignore "cond";
                   if true then ignore "when true" else ignore "when false"
                   ignore "merge"
                   0, Some (waitCond :> obj) @>
            Expect.equal condNode.Expr expectedExpr "condNode.Expr is transformed"

            Expect.isFalse condNode.HasMultipleIncomingEdges "HasMultipleIncomingEdges is not changed"
            Expect.isTrue condNode.ReturnsWaitCondition "condNode.ReturnsWaitCondition <- mergeNode.ReturnsWaitCondition"
            Expect.sequenceEqual condNode.Edges [nextNode] "condNode has an outgoing edge to nextNode"
        }

        test "reduceWhile; the exit node returns WaitCondition" {
            let waitCond = Unchecked.defaultof<WaitCondition<int, unit>>
            let prevNode = lambdaToNode oneWayLambda
            let condNode = lambdaToNode <@ fun _ -> ignore "cond"; (if true then 1 else 0), None @>
            condNode.HasMultipleIncomingEdges <- true
            let loopBodyNode = lambdaToNode <@ fun _ -> ignore "loop"; 0, None @>
            let exitNode = lambdaToNode <@ fun _ -> ignore "exit"; 0, Some (waitCond :> obj) @>
            let nextNode = lambdaToNode oneWayLambda

            connectMutNode(prevNode, condNode)
            connectMutNode(condNode, exitNode)
            connectMutNode(condNode, loopBodyNode)
            connectMutNode(loopBodyNode, condNode)
            connectMutNode(exitNode, nextNode)

            let incomingEdges = Dictionary()
            incomingEdges.Add(prevNode, HashSet())
            incomingEdges.Add(condNode, HashSet([prevNode; loopBodyNode]))
            incomingEdges.Add(loopBodyNode, HashSet([condNode]))
            incomingEdges.Add(exitNode, HashSet([condNode]))
            incomingEdges.Add(nextNode, HashSet([exitNode]))

            Expect.isTrue condNode.HasMultipleIncomingEdges "condNode has multiple incoming edges before reducing"
            Expect.isFalse condNode.ReturnsWaitCondition "condNode does not return WaitCondition before reducing"

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
                   0, Some (waitCond :> obj) @>
            Expect.equal condNode.Expr expectedExpr "condNode.Expr is transformed"

            Expect.isFalse condNode.HasMultipleIncomingEdges "the edge from loopBodyNode to condNode is removed"
            Expect.isTrue condNode.ReturnsWaitCondition "condNode.ReturnsWaitCondition <- exitNode.ReturnsWaitCondition"
            Expect.sequenceEqual condNode.Edges [nextNode] "condNode has an outgoing edge to nextNode"
        }
    ]
