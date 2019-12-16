namespace PopDEVS.ProcessOriented

open PopDEVS

[<AbstractClass>]
type internal WaitConditionInner() =
    /// <param name="args">env, boxedInputBuffer</param>
    abstract member Poll : args: (ISimEnv * obj) -> unit
    abstract member TimeAdvance : now: float -> float
    member val Result = None with get, set

type internal TimeoutCondition(endTime: float) =
    inherit WaitConditionInner()

    override this.Poll((env, _)) =
        if SimEnv.getTime env >= endTime then
            this.Result <- box () |> Some

    override _.TimeAdvance(now) =
        endTime - now

type internal ReceiveEventCondition<'a, 'b>(chooser: InputEventChooser<'a, 'b>) =
    inherit WaitConditionInner()

    override this.Poll((_, inputBuf)) =
        if Option.isNone this.Result then
            this.Result <-
                inputBuf |> unbox
                |> InputEventBuffer.takeOne chooser
                |> Option.map box

    override this.TimeAdvance(_) =
        match this.Result with
        | Some _ -> 0.0
        | None -> infinity

type internal OrWaitCondition<'a, 'b>(left: WaitConditionInner, right: WaitConditionInner) =
    inherit WaitConditionInner()

    override this.Poll(args) =
        if Option.isNone left.Result then
            left.Poll(args)

        if Option.isNone right.Result then
            right.Poll(args)

        match left.Result, right.Result with
        | None, None -> ()
        | l, r ->
            this.Result <-
                (Option.map unbox<'a> l, Option.map unbox<'b> r)
                |> box |> Some

    override this.TimeAdvance(now) =
        match this.Result with
        | Some _ -> 0.0
        | None -> min (left.TimeAdvance(now)) (right.TimeAdvance(now))

type internal AndWaitCondition<'a, 'b>(left: WaitConditionInner, right: WaitConditionInner) =
    inherit WaitConditionInner()

    override this.Poll(args) =
        if Option.isNone left.Result then
            left.Poll(args)

        if Option.isNone right.Result then
            right.Poll(args)

        match left.Result, right.Result with
        | Some x, Some y ->
            this.Result <-
                (x :?> 'a, y :?> 'b)
                |> box |> Some
        | _ -> ()

    override _.TimeAdvance(now) =
        match left.Result, right.Result with
        | Some _, Some _ -> 0.0
        | None, Some _ -> left.TimeAdvance(now)
        | Some _, None -> right.TimeAdvance(now)
        | None, None -> max (left.TimeAdvance(now)) (right.TimeAdvance(now))

[<AbstractClass>]
type WaitCondition internal (inner: WaitConditionInner) =
    member internal __.Inner = inner

type WaitCondition<'Input, 'Result> internal (inner: WaitConditionInner) =
    inherit WaitCondition(inner)

[<AutoOpen>]
module WaitCondition =
    /// 左辺または右辺の少なくともどちらかの条件を満たしたときに待機を終了する `WaitCondition<'a, 'b option * 'c option>` を作成します。
    [<CompiledName("Or")>]
    let (|||) (x: WaitCondition<'a, 'b>) (y: WaitCondition<'a, 'c>) =
        WaitCondition<'a, 'b option * 'c option>(OrWaitCondition<'b, 'c>(x.Inner, y.Inner))

    /// 左辺と右辺が条件を満たしたときに待機を終了する `WaitCondition<'a, 'b * 'c>` を作成します。
    [<CompiledName("And")>]
    let (&&&) (x: WaitCondition<'a, 'b>) (y: WaitCondition<'a, 'c>) =
        WaitCondition<'a, 'b * 'c>(AndWaitCondition<'a, 'c>(x.Inner, y.Inner))
