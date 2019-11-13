module internal PopDEVS.ProcessOriented.MutableCfg

open System.Collections.Generic
open System.Collections.Immutable
open FSharp.Quotations

type FsExpr = FSharp.Quotations.Expr
type FsExpr<'T> = FSharp.Quotations.Expr<'T>
type FsVar = FSharp.Quotations.Var

type ImmutableVar = ControlFlowGraph.Variable
type ImmutableNode = ControlFlowGraph.Node
type ImmutableGraph = ControlFlowGraph.Graph

[<ReferenceEquality>]
type MutableVar =
    { FsVar: FsVar
      /// 外部からキャプチャした変数なら、その値を代入
      CapturedValue: obj option
      /// ラムダ式にキャプチャされる変数か
      mutable IsEscaped: bool }

[<ReferenceEquality>]
type MutableNode =    
    { /// 前回のイベントの戻り値を受け取る obj 型変数
      mutable LambdaParameter: FsVar
      /// 処理を行い、次に遷移する辺のインデックスを返す式
      mutable Expr: FsExpr<int * WaitCondition option>
      /// 複数の入力辺が存在する可能性があるか
      mutable HasMultipleIncomingEdges: bool
      mutable ReturnsWaitCondition: bool
      Edges: List<MutableNode> }

// TODO: こうするべきだった案
// LambdaParameter: FsVar option
// IncomingEdges: HashSet<MutableNode>
// OutgoingEdges: List<(MutableNode * returnsWaitCondition: bool)>

/// `FSharp.Quotations.Expr` を `FSharp.Quotations.Expr<int * WaitCondition option>` に変換する
let excast (source: FsExpr) =
    match source with
    | :? FsExpr<int * WaitCondition option> as x -> x
    | x -> FsExpr.Cast<int * WaitCondition option>(x)

/// Sequential で連結された最後の式と、それ以外に分離する
let rec splitLastExpr = function
    | Patterns.Sequential (left, right) ->
        match splitLastExpr right with
        | Some proc, last -> Some (FsExpr.Sequential (left, proc)), last
        | None, last -> Some left, last
    | x -> None, x

let (|OneWayBody|_|) expr =
    let (|OneWayExpr|_|) = function
        | Patterns.NewTuple [edgeIndexExpr; waitCondOptionExpr]
            when waitCondOptionExpr = <@@ Option<WaitCondition>.None @@> ->
            match edgeIndexExpr with
            | Patterns.ValueWithName _ ->
                // ValueWithName は定数なので DerivedPatterns.Int32 にマッチするが
                // 定数ではなく変数として扱いたいので、先に引っ掛ける。
                None
            | DerivedPatterns.Int32 0 -> Some()
            | _ -> None
        | _ -> None

    let body, last = splitLastExpr expr
    match last with
    | OneWayExpr -> Some body
    | _ -> None

let (|ReturnsWaitCondition|_|) expr =
    let _, lastExpr = splitLastExpr expr
    match lastExpr with
    | Patterns.NewTuple [_; Patterns.NewUnionCase (caseInfo, caseArgs)] ->
        match caseInfo.Name, caseArgs with
        | "Some", [_] -> Some () // Some を返す = WaitCondition を返す
        | _ -> None
    | _ ->
        invalidArg (nameof expr) "The last expression is not NewTuple."

/// `node.Expr` が `node.LambdaParameter` を参照しているならば `true` を返す
let refToParam node =
    node.Expr.GetFreeVars() |> Seq.contains node.LambdaParameter

let connectMutNode (left, right) = left.Edges.Add(right)

let enumerateNodes rootNode =
    let nodeList = List()
    let nodeSet = HashSet()

    let rec traverse node =
        if nodeSet.Add(node) then
            nodeList.Add(node)
            node.Edges |> Seq.iter traverse

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

        for index in 0 .. nodes.Count - 1 do
            let mutNode = nodes.[index]

            edgesBuilder.Capacity <- mutNode.Edges.Count
            mutNode.Edges
                |> Seq.map (fun x -> nodeDic.[x])
                |> edgesBuilder.AddRange

            let imNode: ImmutableNode =
                { Index = index
                  LambdaParameter = mutNode.LambdaParameter
                  Expr = mutNode.Expr
                  HasMultipleIncomingEdges = mutNode.HasMultipleIncomingEdges
                  Edges = edgesBuilder.MoveToImmutable() }
            nodesBuilder.Add(imNode)

        nodesBuilder.MoveToImmutable()

    { Variables = ImmutableArray.CreateRange(vars)
      Nodes = nodes }

let createMutableNodes (graph: ImmutableGraph) =
    let mutableNodes =
        let toMutableNode (x: ImmutableNode) =
            let returnsWaitCondition =
                match x.Expr with
                | ReturnsWaitCondition -> true
                | _ -> false
            { LambdaParameter = x.LambdaParameter
              Expr = x.Expr
              HasMultipleIncomingEdges = x.HasMultipleIncomingEdges
              ReturnsWaitCondition = returnsWaitCondition
              Edges = List() }
        graph.Nodes |> Seq.map toMutableNode |> Seq.toArray

    for i in 0 .. mutableNodes.Length - 1 do
        let edges =
            graph.Nodes.[i].Edges
            |> Seq.map (fun x -> mutableNodes.[x])
        mutableNodes.[i].Edges.AddRange(edges)

    mutableNodes
