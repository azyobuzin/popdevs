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

/// Sequential で連結された最後の式と、それ以外に分離する
let rec splitLastExpr = function
    | Patterns.Sequential (left, right) ->
        match splitLastExpr right with
        | Some proc, last -> Some (FsExpr.Sequential (left, proc)), last
        | None, last -> Some left, last
    | x -> None, x

let oneWay = <@ 0, Option<WaitCondition>.None @>

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
