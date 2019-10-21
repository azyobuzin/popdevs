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

    override this.ToString() =
        let sb = StringBuilder().AppendFormat("{0}: {1}", this.FsVar.Name, this.FsVar.Type)
        if this.CapturedValue.IsSome || this.IsEscaped then
            sb.Append(" (") |> ignore
            match this.CapturedValue with
                | Some x -> sb.AppendFormat("CapturedValue = {0}", x) |> ignore
                | None -> ()
            if this.IsEscaped then
                if this.CapturedValue.IsSome then sb.Append(", ") |> ignore
                sb.Append("IsEscaped = true") |> ignore
            sb.Append(')') |> ignore
        sb.ToString()

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

    override this.ToString() =
        let sb =
            StringBuilder()
                .AppendFormat("Variables = ")
                .AppendLine(String.Join(", ", this.Variables))
                .AppendLine()

        for i = 0 to this.Nodes.Length - 1 do
            let node = this.Nodes.[i]
            let firstLine = sprintf "=== Node %d ===" i
            sb.AppendLine(firstLine)
                .Append("HasMultipleIncomingEdges = ")
                .Append(node.HasMultipleIncomingEdges)
                .AppendLine()
                .Append(node.LambdaParameter.Name)
                .AppendLine(" ->")
                .AppendLine(string node.Expr)
                .Append("Edges: ")
                .AppendLine(String.Join(", ", node.Edges))
                .Append('=', firstLine.Length)
                .AppendLine().AppendLine() |> ignore

        sb.ToString().TrimEnd()
