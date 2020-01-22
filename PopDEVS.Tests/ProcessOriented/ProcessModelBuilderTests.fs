module PopDEVS.Tests.ProcessOriented.ProcessModelBuilderTests

open System
open System.Collections.Immutable
open System.Reflection
open Expecto
open FSharp.Quotations
open PopDEVS.ProcessOriented
open PgUtils

let private createImmutableNode index edges (expr: FsExpr<obj -> int * WaitCondition option>) : ImmutableNode =
    match expr with
    | Patterns.Lambda (lambdaVar, lambdaBody) ->
        { Index = index
          LambdaParameter = Some lambdaVar
          Expr = lambdaBody |> excast
          Edges = ImmutableArray.CreateRange(edges) }
    | _ -> invalidArg (nameof expr) "expr is not a lambda expression."

let rec private exprEqual varMap leftExpr rightExpr =
    match leftExpr, rightExpr with
    | Patterns.Let (lv, le, lb), Patterns.Let (rv, re, rb) ->
        exprEqual varMap le re || exprEqual (Map.add lv rv varMap) lb rb
    | Patterns.Let _, _ | _, Patterns.Let _ -> false
    | Patterns.LetRecursive (lvs, lb), Patterns.LetRecursive (rvs, rb) ->
        let varMapOpt =
            let rec updateVarMap varMap = function
                | (lv, _) :: lvs, (rv, _) :: rvs ->
                    updateVarMap (Map.add lv rv varMap) (lvs, rvs)
                | [], [] -> Some varMap // A number of the elements is same
                | _ -> None
            updateVarMap varMap (lvs, rvs)
        match varMapOpt with
        | Some varMap ->
            let rec checkExprs = function
                | (_, le) :: lvs, (_, re) :: rvs ->
                    exprEqual varMap le re && checkExprs (lvs, rvs)
                | [], [] -> true
                | _ -> failwith "unreachable"
            checkExprs (lvs, rvs) && exprEqual varMap lb rb
        | None -> false
    | Patterns.LetRecursive _, _ | _, Patterns.LetRecursive _ -> false
    | ExprShape.ShapeVar lv, ExprShape.ShapeVar rv ->
        match Map.tryFind lv varMap with
        | Some x -> x = rv
        | _ -> lv = rv
    | ExprShape.ShapeLambda (lv, le), ExprShape.ShapeLambda (rv, re) ->
        exprEqual (Map.add lv rv varMap) le re
    | ExprShape.ShapeCombination (ls, les), ExprShape.ShapeCombination(rs, res) ->
        let getExprConstInfo shape =
            shape.GetType().InvokeMember("Item1",
                BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.GetProperty,
                null, shape, null)
        let rec checkExprs = function
            | le :: les, re :: res ->
                exprEqual varMap le re && checkExprs (les, res)
            | [], [] -> true // A number of the elements is same
            | _ -> false
        obj.Equals(getExprConstInfo ls, getExprConstInfo rs) && checkExprs (les, res)
    | _ -> false

let private expectNodeEqual (actual: ImmutableNode) (expected: ImmutableNode) index =
    let msg propName =
        String.Format("actualNodes.[{0}].{1} = expectedNodes.[{0}].{1}",
                      index, propName)

    Expect.equal
        actual.Index expected.Index
        (msg (nameof expected.Index))

    let varMap =
        [
            match actual.LambdaParameter, expected.LambdaParameter with
            | Some actualParam, Some expectedParam ->
                yield actualParam, expectedParam
            | _ -> ()
        ] |> Map.ofList

    Expect.isTrue
        (exprEqual varMap actual.Expr expected.Expr)
        (msg (nameof expected.Expr))

    Expect.sequenceEqual
        actual.Edges expected.Edges
        (msg (nameof expected.Edges))

[<Tests>]
let tests =
    testList "ProcessGraphBuilder" [
        test "convert computation expression with loop and bind to process graph" {
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

            let graph = ProcessGraphBuilder.build builderResult
            let actualNodes = graph.Nodes

            let getVar name =
                graph.Variables
                |> Seq.map (fun x -> x.FsVar)
                |> Seq.find (fun x -> x.Name = name)

            let iVar = getVar "i"
            let iExpr = FsExpr.Cast<int>(FsExpr.Var(iVar))
            let returnInputWaitConditionExpr = FsExpr.Cast<WaitCondition<int, int>>(FsExpr.Var(getVar (nameof returnInputWaitCondition)))
            let unitWaitConditionExpr = FsExpr.Cast<WaitCondition<int, unit>>(FsExpr.Var(getVar (nameof unitWaitCondition)))

            let expectedNodes =
                [
                    createImmutableNode 0 [1]
                        <@ fun _ -> %%(FsExpr.VarSet(iVar, <@@ 1 @@>)); 0, Option<WaitCondition>.None @>

                    createImmutableNode 1 [2]
                        <@ fun _ -> (if %iExpr <= 9 then 0 else 1), Option<WaitCondition>.None @>

                    createImmutableNode 2 [3]
                        <@ fun _ -> 0, Some (%returnInputWaitConditionExpr :> WaitCondition) @>

                    createImmutableNode 3 [4; 5]
                        <@ fun waitResult ->
                            (if (let compilerGeneratedVar = waitResult |> unbox<int>
                                 let v = compilerGeneratedVar
                                 v % 2 = 0) then 0 else 1), Option<WaitCondition>.None @>
                            
                    createImmutableNode 4 [5]
                        <@ fun _ -> 0, Some (%unitWaitConditionExpr :> WaitCondition) @>

                    createImmutableNode 5 [1]
                        <@ fun _ ->
                            %%(FsExpr.VarSet(iVar, <@@ %iExpr + 1 @@>))
                            0, Option<WaitCondition>.None @>
                ]

            Expect.hasLength actualNodes expectedNodes.Length "graph has 7 nodes"

            Seq.zip actualNodes expectedNodes
            |> Seq.iteri (fun i (actual, expected) ->
                 expectNodeEqual actual expected i)
        }
    ]
