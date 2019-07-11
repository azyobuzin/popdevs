module internal PopDEVS.SequentialCoordinators

open System.Collections.Generic

[<AbstractClass>]
type Coordinator() =
    abstract member Initialize : initialTime: float -> unit
    abstract member CollectOutputs : time: float -> obj seq
    abstract member Inbox : event: obj * time: float -> unit
    abstract member AdvanceSimulation : time: float -> unit

[<Sealed>]
type AtomicModelSimulator(model: AtomicModel.BoxedAtomicModel) =
    inherit Coordinator()

    let mutable lastTime = 0.0
    let mutable nextTime = 0.0
    let mutable externalTime = 0.0
    let inbox = List<obj>()

    override __.Initialize(initialTime) =
        lastTime <- initialTime
        nextTime <- initialTime + model.TimeAdvance()

    override __.CollectOutputs(time) =
        assert (time >= lastTime && time <= nextTime)

        if time = nextTime then
            model.Output()
        else
            Seq.empty

    override __.Inbox(event, time) =
        if inbox.Count = 0 then
            assert (time >= lastTime && time <= nextTime)
            externalTime <- time
        else
            assert (time = externalTime)

        inbox.Add(event)

    override __.AdvanceSimulation(time) =
        assert (time = nextTime || (inbox.Count > 0 && externalTime = time))

        let elapsed = { Completed = time = nextTime; Elapsed = time - nextTime }
        model.Transition(elapsed, inbox :> obj seq)
        inbox.Clear()
        lastTime <- time
        nextTime <- time + model.TimeAdvance()

[<Sealed>]
type CoupledModelSimulator(model: CoupledModel<_, _>) =
    inherit Coordinator()

    // TODO: subcoordinators を引数に取らないと……
    let mutable futureEventList = [0.0]
    let mutable lastProcessedTime = 0.0

    override __.Initialize(initialTime) =
        raise (System.NotImplementedException())

    override __.CollectOutputs(time) =
        raise (System.NotImplementedException())

    override __.Inbox(event, time) =
        raise (System.NotImplementedException("translation して subcoordinator に転送"))

    override __.AdvanceSimulation(time) =
        assert (time >= lastProcessedTime && time <= List.head futureEventList)
        raise (System.NotImplementedException())
