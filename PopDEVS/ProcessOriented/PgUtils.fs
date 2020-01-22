module internal PopDEVS.ProcessOriented.PgUtils

open System.Collections.Generic
open System.Collections.Immutable
open FSharp.Quotations

type FsExpr = FSharp.Quotations.Expr
type FsExpr<'T> = FSharp.Quotations.Expr<'T>
type FsVar = FSharp.Quotations.Var

type ImmutableVar = ProcessGraph.Variable
type ImmutableNode = ProcessGraph.Node
type ImmutableGraph = ProcessGraph.Graph

[<ReferenceEquality>]
type MutableNode =    
    { /// 前回のイベントの戻り値を受け取る obj 型変数
      LambdaParameter: FsVar option
      /// 処理を行い、次に遷移する辺のインデックスを返す式
      Expr: FsExpr<int * WaitCondition option>
      IncomingEdges: HashSet<MutableNode>
      /// 出力辺
      OutgoingEdges: List<MutableNode> }

let connectMutNode left right =
    left.OutgoingEdges.Add(right)
    right.IncomingEdges.Add(left) |> ignore

let connectOpt left right =
    match left, right with
    | Some l, Some r -> connectMutNode l r
    | None, Some _ -> invalidArg (nameof left) "left is None although right is Some."
    | _ -> ()

let enumerateNodes rootNode =
    let nodeList = List()
    let nodeSet = HashSet()

    let rec traverse node =
        if nodeSet.Add(node) then
            nodeList.Add(node)
            node.OutgoingEdges |> Seq.iter traverse

    traverse rootNode
    nodeList :> IReadOnlyList<MutableNode>

let createImmutableGraph (vars, rootNode) : ImmutableGraph =
    let nodes =
        let nodes = enumerateNodes rootNode
        let nodeDic =
            let dic = Dictionary(nodes.Count)
            for i in 0 .. nodes.Count - 1 do
                dic.Add(nodes.[i], i)
            dic

        let nodesBuilder = ImmutableArray.CreateBuilder(nodes.Count)
        let edgesBuilder = ImmutableArray.CreateBuilder(0)

        for index = 0 to nodes.Count - 1 do
            let mutNode = nodes.[index]

            edgesBuilder.Capacity <- mutNode.OutgoingEdges.Count
            mutNode.OutgoingEdges
                |> Seq.map (fun x -> nodeDic.[x])
                |> edgesBuilder.AddRange

            let imNode: ImmutableNode =
                { Index = index
                  LambdaParameter = mutNode.LambdaParameter
                  Expr = mutNode.Expr
                  Edges = edgesBuilder.MoveToImmutable() }
            nodesBuilder.Add(imNode)

        nodesBuilder.MoveToImmutable()

    { Variables = ImmutableArray.CreateRange(vars)
      Nodes = nodes }

/// `FSharp.Quotations.Expr` を `FSharp.Quotations.Expr<int * WaitCondition option>` に変換する
let excast (source: FsExpr) =
    match source with
    | :? FsExpr<int * WaitCondition option> as x -> x
    | x -> FsExpr.Cast<int * WaitCondition option>(x)

let unitExpr = <@@ () @@>

let private unboxMethod =
    match <@@ unbox null @@> with
    | Patterns.Call (_, methodInfo, _) ->
        methodInfo.GetGenericMethodDefinition()
    | _ -> failwith "unreachable"
/// `obj` を指定した型 `ty` に変換する
let unboxExpr ty expr =
    FsExpr.Call(unboxMethod.MakeGenericMethod([| ty |]), [expr])

let oneWay = <@ 0, Option<WaitCondition>.None @>

let continueWithLet var expr body =
    let rec f = function
        | Patterns.Sequential (x, y) ->
            FsExpr.Sequential(x, f y)
        | Patterns.Let (x, y, z) ->
            FsExpr.Let(x, y, f z)
        | Patterns.LetRecursive (x, y) ->
            FsExpr.LetRecursive(x, f y)
        | x ->
            FsExpr.Let(var, x, body)
    f expr

let mkSeqExpr first second =
    let rec f = function
        | Patterns.Sequential (x, y) ->
            FsExpr.Sequential(x, f y)
        | Patterns.Let (x, y, z) ->
            FsExpr.Let(x, y, f z)
        | Patterns.LetRecursive (x, y) ->
            FsExpr.LetRecursive(x, f y)
        | x ->
            FsExpr.Sequential(x, second)
    f first

type LetRecKind =
    | NotRecursive
    | Recursive
    | MutuallyRecursive

let bindingsWithKind (bindings: (FsVar * FsExpr) list) =
    let varSet = bindings |> Seq.map fst |> set
    let k self =
        let otherVars = varSet |> Set.remove self
        let rec exprKind = function
            | ExprShape.ShapeVar v ->
                if otherVars |> Set.contains v then MutuallyRecursive
                elif v = self then Recursive
                else NotRecursive
            | ExprShape.ShapeLambda (_, x) -> exprKind x
            | ExprShape.ShapeCombination (_, exprs) ->
                let rec kmany = function
                    | e :: es ->
                        let x = exprKind e
                        if x = MutuallyRecursive then MutuallyRecursive
                        else
                            let y = kmany es
                            match y with
                            | Recursive | MutuallyRecursive -> y
                            | NotRecursive -> x
                    | [] -> NotRecursive
                kmany exprs
        exprKind
    bindings |> List.map (fun ((v, e) as t) -> t, k v e)
