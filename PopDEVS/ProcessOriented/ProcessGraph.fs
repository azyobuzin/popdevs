module PopDEVS.ProcessOriented.ProcessGraph

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
      CapturedValue: obj option }

    override this.ToString() =
        let sb = StringBuilder().AppendFormat("{0}: {1}", this.FsVar.Name, this.FsVar.Type)
        this.CapturedValue |> Option.iter (fun x ->
            sb.AppendFormat(" (CapturedValue = {0})", x) |> ignore)
        sb.ToString()

[<ReferenceEquality>]
type Node =
    { Index: int
      /// 前回のイベントの戻り値を受け取る obj 型変数
      LambdaParameter: FsVar option
      /// 処理を行い、次に遷移する辺のインデックスとイベントを返す式
      Expr: FsExpr<int * WaitCondition option>
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
                .AppendLine()
                .Append(match node.LambdaParameter with
                        | Some x -> x.Name | None -> "_")
                .AppendLine(" ->")
                .AppendLine(string node.Expr)
                .Append("Edges: ")
                .AppendLine(String.Join(", ", node.Edges))
                .Append('=', firstLine.Length)
                .AppendLine().AppendLine() |> ignore

        sb.ToString().TrimEnd()
