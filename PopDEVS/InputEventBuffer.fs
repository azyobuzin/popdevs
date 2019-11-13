namespace PopDEVS

open System.Collections.Immutable

type ReceivedEvent<'a> =
    { Time: float; Event: 'a }

    static member getTime (re: ReceivedEvent<'a>) =
        re.Time

    static member getEvent (re: ReceivedEvent<'a>) =
        re.Event

type InputEventChooser<'a, 'b> = ReceivedEvent<'a> -> 'b option

type internal IInputEventBuffer =
    abstract Take : chooser: (InputEventChooser<obj, 'a>) * limit: int option -> ImmutableArray<'a>

type InputEventBuffer<'a> internal (impl: IInputEventBuffer) =
    static let unboxEvent (re: ReceivedEvent<obj>) =
        { Time = re.Time; Event = unbox re.Event }

    member __.Take(chooser: InputEventChooser<'a, 'b>, limit) =
        impl.Take(unboxEvent >> chooser, limit)

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
        | _ -> failwith "takeWithLimit returns more than 1 events."

[<AutoOpen>]
module InputEventChooser =
    /// <example><code>let event = InputEventBuffer.take anyEvent inputBuf</code></example>
    let anyEvent re = Some (ReceivedEvent.getEvent re)
