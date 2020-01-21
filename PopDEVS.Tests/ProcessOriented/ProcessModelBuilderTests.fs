module PopDEVS.Tests.ProcessOriented.ProcessModelBuilderTests

open System
open System.Collections.Immutable
open Expecto
open FSharp.Quotations
open PopDEVS.ProcessOriented
open MutableCfg

let private createImmutableNode (index, hasMultipleIncomingEdges, edges) (expr: FsExpr<obj -> int * WaitCondition option>) : ImmutableNode =
    match expr with
    | Patterns.Lambda (lambdaVar, lambdaBody) ->
        { Index = index
          LambdaParameter = lambdaVar
          Expr = lambdaBody |> excast
          Edges = ImmutableArray.CreateRange(edges) }
    | _ -> invalidArg (nameof expr) "expr is not a lambda expression."

let private expectNodeEqual (actual: ImmutableNode) (expected: ImmutableNode) index =
    let msg propName =
        String.Format("actualNodes.[{0}].{1} = expectedNodes.[{0}].{1}",
                      index, propName)

    Expect.equal
        actual.Index expected.Index
        (msg (nameof expected.Index))

    // ラムダパラメータを書き換えて、 Equals が成立するようにする
    let actualExpr =
        let substitution var =
            if var = actual.LambdaParameter then
                Some (FsExpr.Var(expected.LambdaParameter))
            else
                None
        actual.Expr.Substitute(substitution)

    Expect.equal
        actualExpr expected.Expr.Raw
        (msg (nameof expected.Expr))

    Expect.sequenceEqual
        actual.Edges expected.Edges
        (msg (nameof expected.Edges))

[<Tests>]
let tests =
    testList "ProcessModelBuilder" [
        test "convert computation expression with loop and bind to CFG" {
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
            let actualNodes = graph.Nodes

            let getVar name =
                graph.Variables
                |> Seq.map (fun x -> x.FsVar)
                |> Seq.find (fun x -> x.Name = name)

            let iVar = getVar "i"
            let iExpr = FsExpr.Cast<int>(FsExpr.Var(iVar))
            let vVar = getVar "v"
            let vExpr = FsExpr.Cast<int>(FsExpr.Var(vVar))
            let returnInputWaitConditionExpr = FsExpr.Cast<WaitCondition<int, int>>(FsExpr.Var(getVar (nameof returnInputWaitCondition)))
            let unitWaitConditionExpr = FsExpr.Cast<WaitCondition<int, unit>>(FsExpr.Var(getVar (nameof unitWaitCondition)))

            let expectedNodes =
                [
                    createImmutableNode (0, false, [1])
                        <@ fun _ -> %%(FsExpr.VarSet(iVar, <@@ 1 @@>)); 0, Option<WaitCondition>.None @>

                    createImmutableNode (1, true, [2; 3])
                        <@ fun _ -> (if %iExpr <= 9 then 1 else 0), Option<WaitCondition>.None @>

                    createImmutableNode (2, false, [])
                        <@ fun _ -> 0, Option<WaitCondition>.None @>

                    createImmutableNode (3, false, [4])
                        <@ fun _ -> 0, Some (%returnInputWaitConditionExpr :> WaitCondition) @>

                    createImmutableNode (4, false, [5; 6])
                        (let waitResultVar = FsVar("waitResult", typeof<obj>)
                         FsExpr.Cast<obj -> int * WaitCondition option>(
                            FsExpr.Lambda(waitResultVar,
                                FsExpr.Sequential(
                                    FsExpr.VarSet(vVar, FsExpr.Var(waitResultVar) |> unboxExpr typeof<int>),
                                    <@@ (if %vExpr % 2 = 0 then 1 else 0), Option<WaitCondition>.None @@>))))

                    createImmutableNode (5, true, [1])
                        <@ fun _ ->
                            %%(FsExpr.VarSet(iVar, <@@ %iExpr + 1 @@>))
                            0, Option<WaitCondition>.None @>

                    createImmutableNode (6, false, [5])
                        <@ fun _ -> 0, Some (%unitWaitConditionExpr :> WaitCondition) @>
                ]

            Expect.hasLength actualNodes expectedNodes.Length "graph has 7 nodes"

            Seq.zip actualNodes expectedNodes
            |> Seq.iteri (fun i (actual, expected) ->
                 expectNodeEqual actual expected i)
        }
    ]
