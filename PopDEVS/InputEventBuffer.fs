namespace PopDEVS

open System.Collections.Immutable

type ReceivedEvent<'a> =
    { Time: float; Event: 'a }

    static member getTime (re: ReceivedEvent<'a>) =
        re.Time

    static member getEvent (re: ReceivedEvent<'a>) =
        re.Event

type internal IInputEventBuffer =
    abstract Take : chooser: (ReceivedEvent<obj> -> 'a option) * limit: int option -> ImmutableArray<'a>

type InputEventBuffer<'a> internal (impl: IInputEventBuffer) =
    static let unboxEvent (re: ReceivedEvent<obj>) =
        { Time = re.Time; Event = unbox re.Event }

    member __.Take(chooser: ReceivedEvent<'a> -> 'b option, limit: int option) =
        impl.Take(unboxEvent >> chooser, limit)

module InputEventBuffer =
    /// <summary>入力イベントバッファーから、条件を満たすイベントを取り出します。</summary>
    /// <param name="chooser">条件を指定します。この関数で、シミュレーションの状態を変化させる副作用を起こしてはいけません。</param>
    /// <returns>条件を満たすイベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("Take")>]
    let take chooser (inputBuf: InputEventBuffer<_>) =
        inputBuf.Take(chooser, None)

    /// <summary>入力イベントバッファーから、条件を満たすイベントを取り出します。</summary>
    /// <param name="filter">条件を指定します。この関数で、シミュレーションの状態を変化させる副作用を起こしてはいけません。</param>
    /// <returns>条件を満たすイベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("TakeWithFilter")>]
    let takeWithFilter filter (inputBuf: InputEventBuffer<_>) =
        take (fun x -> if filter x then Some x else None) inputBuf

    /// <summary>入力イベントバッファーにあるイベントを最大 <paramref name="limit"/> 件取り出します。</summary>
    /// <param name="limit">取り出す最大件数を指定します。 <c>None</c> のとき、すべてのイベントを取り出します。</param>
    /// <returns>イベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("TakeAll")>]
    let takeAll limit (inputBuf: InputEventBuffer<_>) =
        inputBuf.Take(Some, limit)

    /// <summary>入力イベントバッファーから、条件を満たすイベントを、時刻を無視して取り出します。</summary>
    /// <param name="filter">条件を指定します。この関数で、シミュレーションの状態を変化させる副作用を起こしてはいけません。</param>
    /// <returns>条件を満たすイベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("TakeEventsWithFilter")>]
    let takeEventsWithFilter filter (inputBuf: InputEventBuffer<_>) =
        let chooser re =
            let ev = re.Event
            if filter ev then Some ev else None
        take chooser inputBuf

    /// <summary>入力イベントバッファーから、条件を満たすイベントを、最大 <paramref name="limit"/> 件取り出します。</summary>
    /// <param name="chooser">条件を指定します。この関数で、シミュレーションの状態を変化させる副作用を起こしてはいけません。</param>
    /// <returns>条件を満たすイベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("TakeWithLimit")>]
    let takeWithLimit chooser limit (inputBuf: InputEventBuffer<_>) =
        inputBuf.Take(chooser, Some limit)

    /// <summary>入力イベントバッファーから、条件を満たすイベントを、最大 <paramref name="limit"/> 件取り出します。</summary>
    /// <param name="filter">条件を指定します。この関数で、シミュレーションの状態を変化させる副作用を起こしてはいけません。</param>
    /// <returns>条件を満たすイベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("TakeWithFilterAndLimit")>]
    let takeWithFilterAndLimit filter limit (inputBuf: InputEventBuffer<_>) =
        takeWithLimit (fun x -> if filter x then Some x else None) limit inputBuf

    /// <summary>入力イベントバッファーから、条件を満たすイベントを、時刻を無視して、最大 <paramref name="limit"/> 件取り出します。</summary>
    /// <param name="filter">条件を指定します。この関数で、シミュレーションの状態を変化させる副作用を起こしてはいけません。</param>
    /// <returns>条件を満たすイベントの配列。受信時刻の昇順にソートされています。同時刻のイベントについては、順番は保証されていません。</returns>
    [<CompiledName("TakeEventsWithFilterAndLimit")>]
    let takeEventsWithFilterAndLimit filter limit (inputBuf: InputEventBuffer<_>) =
        let chooser re =
            let ev = re.Event
            if filter ev then Some ev else None
        takeWithLimit chooser limit inputBuf
