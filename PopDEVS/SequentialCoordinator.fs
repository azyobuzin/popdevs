module internal PopDEVS.SequentialCoordinator

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
        if inbox.Count = 0 then
            ImmutableArray.Empty
        elif time <> externalTime then
            raise (InvalidOperationException())
        else
            let events = ImmutableArray.CreateRange(inbox)
            inbox.Clear()
            events

[<Sealed>]
type AtomicModelSimulator(model: BoxedAtomicModel) =
    inherit Coordinator()

    let mutable state = model.InitialState
    let mutable lastTime = 0.0

    override this.Initialize(initialTime) =
        lastTime <- initialTime
        this.NextTime <- initialTime + model.TimeAdvance state

    override this.CollectOutputs(time) =
        assert (time >= lastTime && time <= this.NextTime)

        if time = this.NextTime then
            model.Output state
        else
            Seq.empty

    override this.AdvanceSimulation(time) =
        let inbox = this.TakeInbox(time)
        assert (time = this.NextTime || (inbox.Length > 0 && this.ExternalTime = time))

        let elapsed = { Completed = time = this.NextTime; Elapsed = time - this.NextTime }
        state <- model.Transition (state, elapsed, inbox :> obj seq)
        lastTime <- time
        this.NextTime <- time + model.TimeAdvance state

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

let create (model: DevsModel) =
    let rec createFromBoxed (model: BoxedModel) =
        let createAtomic (model: BoxedAtomicModel) =
            AtomicModelSimulator(model)

        let createCoupled (model: BoxedCoupledModel) =
            let componentCount = model.Components.Count
            let indexTable = Dictionary(componentCount)
            let componentIds = Array.zeroCreate componentCount
            let subcoordinatorsBuilder = ImmutableArray.CreateBuilder(componentCount)
            for kvp in model.Components do
                let id, submodel = kvp.Key, kvp.Value
                let index = subcoordinatorsBuilder.Count
                indexTable.Add(id, index)
                componentIds.[index] <- id
                subcoordinatorsBuilder.Add(createFromBoxed submodel.Inner)

            let convertTranslations (dic: ImmutableDictionary<_, _>) =
                dic |> Seq.map (fun kvp -> (indexTable.[kvp.Key], kvp.Value))
                    |> ImmutableArray.CreateRange

            let translations =
                componentIds
                |> Seq.map (fun id ->
                    match model.Translations.TryFind(id) with
                    | Some dic -> convertTranslations dic
                    | None -> ImmutableArray.Empty)
                |> ImmutableArray.CreateRange

            CoupledModelSimulator(
                subcoordinatorsBuilder.MoveToImmutable(),
                translations,
                convertTranslations model.InputTranslations,
                convertTranslations model.OutputTranslations)

        match model with
        | BoxedModel.Atomic x -> createAtomic x :> Coordinator
        | BoxedModel.Coupled x -> createCoupled x :> Coordinator

    createFromBoxed model.Inner
