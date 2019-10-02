module PopDEVS.ProcessOriented.ControlFlowGraph

open System
open System.Collections.Immutable
open System.Text

type private FsExpr<'T> = FSharp.Quotations.Expr<'T>
type private FsVar = FSharp.Quotations.Var

/// 状態として記憶されるべき変数
[<ReferenceEquality>]
type Variable =
    { /// 式中でこの変数を表すのに使用する `FSharp.Quotations.Var`
      FsVar: FsVar
      /// 外部からキャプチャした変数なら、その値
      CapturedValue: obj option
      /// ラムダ式にキャプチャされる変数か
      IsEscaped: bool }

[<ReferenceEquality>]
type Node =
    { Index: int
      /// 前回のイベントの戻り値を受け取る obj 型変数
      LambdaParameter: FsVar
      /// 処理を行い、次に遷移する辺のインデックスとイベントを返す式
      Expr: FsExpr<int * obj option>
      /// 複数の入力辺が存在するか
      HasMultipleIncomingEdges: bool
      /// このノードを始点とする辺の終点ノードのインデックス
      Edges: ImmutableArray<int> }

type Graph =
    { Variables: ImmutableArray<Variable>
      Nodes: ImmutableArray<Node> }

let sprintGraph graph =
    let sb = StringBuilder()

    for i in 0 .. graph.Nodes.Length - 1 do
        let node = graph.Nodes.[i]
        let firstLine = sprintf "=== Node %d ===" i
        sb.AppendLine(firstLine)
            .Append(node.LambdaParameter.Name)
            .AppendLine(" ->")
            .AppendLine(string node.Expr)
            .Append("Edges: ")
            .AppendLine(String.Join(", ", node.Edges))
            .Append('=', firstLine.Length)
            .AppendLine().AppendLine() |> ignore

    sb.ToString()
