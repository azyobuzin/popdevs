module internal PopDEVS.SequentialCoordinators

[<AbstractClass>]
type Coordinator() =
    abstract member CollectOutputs : time: float -> obj seq
    abstract member AdvanceSimulation : time: float -> unit

[<Sealed>]
type CoupledModelSimulator(model: CoupledModel<_, _>) =
    inherit Coordinator()

    // TODO: subcoordinators を引数に取らないと……
    let mutable nextScheduledTime = 0.0
    let mutable lastProcessedTime = 0.0

    override __.CollectOutputs(time) =
        raise (System.NotImplementedException())

    override __.AdvanceSimulation(time) =
        assert (time >= lastProcessedTime && time <= nextScheduledTime)
        raise (System.NotImplementedException())
