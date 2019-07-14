module internal PopDEVS.SequentialCoordinators

open System
open System.Collections.Generic
open System.Collections.Immutable

[<AbstractClass>]
type Coordinator() =
    let mutable externalTime = 0.0
    let inbox = List<obj>()

    member val NextTime = 0.0 with get, set
    member internal __.ExternalTime = externalTime

    abstract member Initialize : initialTime: float -> unit
    abstract member CollectOutputs : time: float -> obj seq
    abstract member AdvanceSimulation : time: float -> unit

    member this.Inbox(event: obj, time: float) =
        if inbox.Count = 0 then
            assert (time <= this.NextTime)
            externalTime <- time
        else
            assert (time = externalTime)

        inbox.Add(event)

    member internal __.TakeInbox(time) =
        if time <> externalTime then
            raise (InvalidOperationException())

        let events = ImmutableArray.CreateRange(inbox)
        inbox.Clear()
        events

[<Sealed>]
type AtomicModelSimulator(model: AtomicModel.BoxedAtomicModel) =
    inherit Coordinator()

    let mutable lastTime = 0.0

    override this.Initialize(initialTime) =
        lastTime <- initialTime
        this.NextTime <- initialTime + model.TimeAdvance()

    override this.CollectOutputs(time) =
        assert (time >= lastTime && time <= this.NextTime)

        if time = this.NextTime then
            model.Output()
        else
            Seq.empty

    override this.AdvanceSimulation(time) =
        let inbox = this.TakeInbox(time)
        assert (time = this.NextTime || (inbox.Length > 0 && this.ExternalTime = time))

        let elapsed = { Completed = time = this.NextTime; Elapsed = time - this.NextTime }
        model.Transition(elapsed, inbox :> obj seq)
        lastTime <- time
        this.NextTime <- time + model.TimeAdvance()

[<Sealed>]
type CoupledModelSimulator(subcoordinators: ImmutableArray<Coordinator>,
                           translations: ImmutableArray<ImmutableArray<int * (obj -> obj option)>>,
                           inputTranslations: ImmutableArray<int * (obj -> obj option)>,
                           outputTranslations: ImmutableArray<int * (obj -> obj option)>) =
    inherit Coordinator()

    let mutable lastTime = 0.0

    override this.Initialize(initialTime) =
        lastTime <- initialTime

        let mutable minNextTime = infinity
        for subcoordinator in subcoordinators do
            subcoordinator.Initialize(initialTime)
            minNextTime <- min minNextTime subcoordinator.NextTime
        this.NextTime <- minNextTime

    override __.CollectOutputs(time) =
        outputTranslations
        |> Seq.collect (fun (index, transFunc) ->
            subcoordinators.[index].CollectOutputs(time)
            |> Seq.choose transFunc)

    override this.AdvanceSimulation(time) =
        assert (time >= lastTime && time <= this.NextTime)

        let synchronizeSet = HashSet()

        let deliverInputs (tran: (int * (obj -> obj option)) seq) events =
            let translate event (destIndex, transFunc) =
                transFunc event
                |> Option.map (fun transformed -> (destIndex, transformed))
            let deliver (destIndex, transformedEvent) =
                subcoordinators.[destIndex].Inbox(transformedEvent, time)
                synchronizeSet.Add(destIndex) |> ignore
            events
            |> Seq.collect (fun event -> tran |> Seq.choose (translate event))
            |> Seq.iter deliver

        let deliverExternalInputs () =
            deliverInputs inputTranslations (this.TakeInbox(time))

        let collectImminents () =
            let imminentSubcoordinators =
                subcoordinators
                |> Seq.indexed
                |> Seq.filter (fun (_, c) -> c.NextTime = time)
            for (imminentIndex, coordinator) in imminentSubcoordinators do
                synchronizeSet.Add(imminentIndex) |> ignore
                deliverInputs translations.[imminentIndex] (coordinator.CollectOutputs(time))

        deliverExternalInputs ()
        collectImminents ()

        let mutable minTime = infinity
        for coordinatorIndex in synchronizeSet do
            let coordinator = subcoordinators.[coordinatorIndex]
            coordinator.AdvanceSimulation(time)
            minTime <- min minTime coordinator.NextTime

        this.NextTime <- minTime
