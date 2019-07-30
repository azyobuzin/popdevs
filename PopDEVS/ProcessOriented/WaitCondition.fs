namespace PopDEVS.ProcessOriented

open PopDEVS

type internal PollFunc<'Input, 'Result> =
    obj * ISimEnv * ElapsedTime * InputEventBuffer<'Input> -> 'Result option

type WaitCondition<'Input, 'Result> internal (poll: PollFunc<'Input, 'Result>) =
    member internal __.Poll(args) =
        poll args

    /// <summary>左辺または右辺の少なくともどちらかの条件を満たしたときに待機を終了する <see cref="WaitCondition{I,O}"/> を作成します。</summary>
    static member (|||) (x: WaitCondition<'Input, 'Result>, y: WaitCondition<'Input, _>) =
        let poll args =
            let xresult = x.Poll(args)
            let yresult = y.Poll(args)
            match xresult, yresult with
            | None, None -> None
            | x -> Some x
        WaitCondition(poll)

    /// <summary>左辺と右辺が条件を満たしたときに待機を終了する <see cref="WaitCondition{I,O}"/> を作成します。</summary>
    static member (&&&) (x: WaitCondition<'Input, 'Result>, y: WaitCondition<'Input, _>) =
        let poll args =
            let xresult = x.Poll(args)
            let yresult = y.Poll(args)
            match xresult, yresult with
            | Some x, Some y -> Some (x, y)
            | _ -> None
        WaitCondition(poll)

module internal WaitCondition =
    // TODO: ProcessEnv のほうに実装する

    let timeout targetTime : PollFunc<_, _> =
        fun (_, env, _, _) ->
            if SimEnv.getTime env >= targetTime then Some () else None

    let inputEvent chooser : PollFunc<_, _> =
        fun (_, _, _, inputBuf) ->
            let events = inputBuf |> InputEventBuffer.takeWithLimit chooser 1
            match events.Length with
            | 0 -> None
            | 1 -> Some events.[0]
            | _ -> failwith "takeWithLimit returns more than 1 events."
