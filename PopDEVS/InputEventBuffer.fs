namespace PopDEVS

open System.Collections.Immutable

type ReceivedEvent<'a> =
    { Time: float; Event: 'a }

type internal IInputEventBuffer =
    abstract Take : chooser: (ReceivedEvent<obj> -> 'a option) -> ImmutableArray<'a>

type InputEventBuffer<'a> internal (impl: IInputEventBuffer) =
    static let unboxEvent (re: ReceivedEvent<obj>) =
        { Time = re.Time; Event = unbox re.Event }

    member __.Take(chooser: ReceivedEvent<'a> -> 'b option) =
        impl.Take(fun x -> unboxEvent x |> chooser)

    member __.Take(filter: ReceivedEvent<'a> -> bool) =
        impl.Take(fun x ->
            let x = unboxEvent x
            if filter x then Some x else None)

    member __.TakeAll() : ImmutableArray<ReceivedEvent<'a>> =
        impl.Take(fun x -> Some (unboxEvent x))

    member __.TakeEvents(chooser: 'a -> 'b option) =
        impl.Take(fun x -> unbox x.Event |> chooser)

    member __.TakeEvents(filter: 'a -> bool) =
        impl.Take(fun x ->
            let ev = unbox x.Event
            if filter ev then Some ev else None)

    member __.TakeAllEvents() : ImmutableArray<'a> =
        impl.Take(fun x -> Some (unbox x.Event))

module InputEventBuffer =
    /// <summary>入力イベントバッファーから、条件を満たすイベントを取り出します。</summary>
    /// <param name="chooser">条件を指定します。この関数で、シミュレーションの状態を変化させる副作用を起こしてはいけません。</param>
    /// <returns>条件を満たすイベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("Take")>]
    let take (chooser: _ -> 'b option) (inputBuf: InputEventBuffer<'a>) =
        inputBuf.Take(chooser)

    /// <summary>入力イベントバッファーから、条件を満たすイベントを取り出します。</summary>
    /// <param name="filter">条件を指定します。この関数で、シミュレーションの状態を変化させる副作用を起こしてはいけません。</param>
    /// <returns>条件を満たすイベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("TakeWithFilter")>]
    let takeWithFilter (filter: _ -> bool) (inputBuf: InputEventBuffer<'a>) =
        inputBuf.Take(filter)

    /// <summary>入力イベントバッファーにある、すべてのイベントを取り出します。</summary>
    /// <returns>イベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("TakeAll")>]
    let takeAll (inputBuf: InputEventBuffer<'a>) =
        inputBuf.TakeAll()

    /// <summary>入力イベントバッファーから、条件を満たすイベントを、時刻を無視して取り出します。</summary>
    /// <param name="chooser">条件を指定します。この関数で、シミュレーションの状態を変化させる副作用を起こしてはいけません。</param>
    /// <returns>条件を満たすイベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("TakeEvents")>]
    let takeEvents (chooser: _ -> 'b option) (inputBuf: InputEventBuffer<'a>) =
        inputBuf.TakeEvents(chooser)

    /// <summary>入力イベントバッファーから、条件を満たすイベントを、時刻を無視して取り出します。</summary>
    /// <param name="chooser">条件を指定します。この関数で、シミュレーションの状態を変化させる副作用を起こしてはいけません。</param>
    /// <returns>条件を満たすイベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("TakeEventsWithFilter")>]
    let takeEventsWithFilter (filter: _ -> bool) (inputBuf: InputEventBuffer<'a>) =
        inputBuf.TakeEvents(filter)

    /// <summary>入力イベントバッファーにある、すべてのイベントを取り出します。</summary>
    /// <returns>イベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("TakeAllEvents")>]
    let takeAllEvents (inputBuf: InputEventBuffer<'a>) =
        inputBuf.TakeAllEvents()
