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
        let mutable xresult, yresult = None, None
        let poll args =
            if Option.isNone xresult then xresult <- x.Poll(args)
            if Option.isNone yresult then yresult <- y.Poll(args)
            match xresult, yresult with
            | Some x, Some y -> Some (x, y)
            | _ -> None
        WaitCondition(poll)
