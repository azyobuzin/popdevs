module internal PopDEVS.SequentialCoordinators

open System
open System.Collections.Generic

type Inbox() =
    let events = List<obj>()
    let mutable nextTime = 0.0

    member __.AddEvent(event: obj, time: float) =
        if events.Count > 0 && nextTime <> time then
            raise (ArgumentException("Unexpected time"))
        events.Add(event)
        nextTime <- time
        ()

    member __.TakeEvents(time: float) =
        if (time > nextTime) then
            raise (ArgumentException("Unexpected time"))
        elif time = nextTime then
            let ret = events.ToArray() :> IReadOnlyCollection<_>
            events.Clear()
            ret
        else
            Array.empty :> IReadOnlyCollection<_>

[<AbstractClass>]
type Coordinator() =
    abstract member Inbox : event: obj * time: float -> unit
    abstract member CollectOutputs : time: float -> obj seq
    abstract member AdvanceSimulation : time: float -> unit

[<Sealed>]
type CoupledModelSimulator(model: CoupledModel<_, _>) =
    inherit Coordinator()

    // TODO: subcoordinators を引数に取らないと……
    let mutable futureEventList = [0.0]
    let mutable lastProcessedTime = 0.0

    override __.Inbox(event, time) =
        raise (System.NotImplementedException("translation して subcoordinator に転送"))

    override __.CollectOutputs(time) =
        raise (System.NotImplementedException())

    override __.AdvanceSimulation(time) =
        assert (time >= lastProcessedTime && time <= List.head futureEventList)
        raise (System.NotImplementedException())
