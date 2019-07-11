namespace PopDEVS

open System
open System.Collections.Immutable

// TODO: EventInbox
// 入力イベントを保存しておくやつ

// TODO: InputEventBag
// ExternalTransition の引数にするやつ
// Simulation.enqueueUnprocessedEvents に渡すと空になる 

type InputEventBag<'a> internal (events: ImmutableArray<'a>) =
    let mutable events = events
    let mutable processed = false

    member __.Events =
        processed <- true
        events

    /// <summary>未処理のイベントを取り出し、 <see cref="Events" /> プロパティから取得できないようにする。</summary>
    member internal this.MoveOut() =
        if processed then
            raise (InvalidOperationException("The events in this bag has already been processed."))
        let ret = this.Events
        events <- ImmutableArray.Empty
        ret
