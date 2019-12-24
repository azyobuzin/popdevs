namespace PopDEVS

open System.Collections.Immutable

type InputEventChooser<'a, 'b> = 'a -> 'b option

type internal IInputEventBuffer =
    abstract Take : chooser: (InputEventChooser<obj, 'a>) * limit: int option -> ImmutableArray<'a>
    // TODO: Peek

type InputEventBuffer<'a> internal (impl: IInputEventBuffer) =
    member __.Take(chooser: InputEventChooser<'a, 'b>, limit) =
        impl.Take(unbox >> chooser, limit)

module InputEventBuffer =
    /// <summary>入力イベントバッファーから、条件を満たすイベントを取り出します。</summary>
    /// <param name="chooser">条件を指定します。この関数で、シミュレーションの状態を変化させる副作用を起こしてはいけません。</param>
    /// <returns>条件を満たすイベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("Take")>]
    let take chooser (inputBuf: InputEventBuffer<_>) =
        inputBuf.Take(chooser, None)

    /// <summary>入力イベントバッファーから、条件を満たすイベントを、最大 <paramref name="limit"/> 件取り出します。</summary>
    /// <param name="chooser">条件を指定します。この関数で、シミュレーションの状態を変化させる副作用を起こしてはいけません。</param>
    /// <returns>条件を満たすイベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("TakeWithLimit")>]
    let takeWithLimit chooser limit (inputBuf: InputEventBuffer<_>) =
        inputBuf.Take(chooser, Some limit)

    /// <summary>入力イベントバッファーから、条件を満たす最古のイベントを取り出します。</summary>
    /// <param name="chooser">条件を指定します。この関数で、シミュレーションの状態を変化させる副作用を起こしてはいけません。</param>
    let takeOne chooser (inputBuf: InputEventBuffer<_>) =
        let events = inputBuf |> takeWithLimit chooser 1
        match events.Length with
        | 0 -> None
        | 1 -> Some events.[0]
        | _ -> failwith "takeWithLimit returned more than 1 events."

[<AutoOpen>]
module InputEventChooser =
    /// <summary>イベントが <paramref name="cond"/> を満たすなら、 <c>Some ev</code> を返します。</summary>
    /// <example><code>let event = InputEventBuffer.take (eventIf (fun e -> e = x)) inputBuf</code></example>
    let eventIf cond ev = if cond ev then Some ev else None

    /// <summary>イベントが <paramref name="cond"/> を満たすなら、 <c>Some ()</code> を返します。</summary>
    /// <example><code>let event = InputEventBuffer.take (eventIfUnit (fun e -> e = x)) inputBuf</code></example>
    let eventIfUnit cond ev = if cond ev then Some () else None
