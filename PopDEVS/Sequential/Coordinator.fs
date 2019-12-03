module internal PopDEVS.Sequential.Coordinator

open System.Collections.Generic
open System.Collections.Immutable
open PopDEVS

type private InputEventBufferImpl() =
    let events = LinkedList<ReceivedEvent<obj>>()

    member __.IsEmpty =
        events.Count = 0

    member this.Add(event: obj, time: float) =
        if not this.IsEmpty && events.Last.Value.Time > time then
            invalidOp "Cannot add an event older than the last event."

        events.AddLast({ Time = time; Event = event }) |> ignore

    interface IInputEventBuffer with
        member __.Take(chooser, limit) =
            let resultBuilder = ImmutableArray.CreateBuilder()

            /// Returns true if the number of the results is less than limit
            let limitTest () =
                match limit with
                | Some x -> resultBuilder.Count < x
                | None -> true

            let mutable node = events.First
            while (not (isNull node)) && (limitTest ()) do
                let nextNode = node.Next
                match chooser node.Value with
                | Some x ->
                    resultBuilder.Add(x)
                    events.Remove(node)
                | None -> ()
                node <- nextNode
            resultBuilder.ToImmutable()

type private SimEnvImpl(time: float) =
    interface ISimEnv with
        member __.GetTime() =
            time

        member __.RunIO(action) =
            action ()

[<AbstractClass>]
type Coordinator() =
    member val NextTime = 0.0 with get, set

    abstract member Initialize : initialTime: float -> unit
    abstract member CollectOutputs : time: float -> obj seq
    abstract member Inbox : event: obj * time: float -> unit
    abstract member AdvanceSimulation : time: float -> unit

[<Sealed>]
type AtomicModelSimulator(model: BoxedAtomicModel) =
    inherit Coordinator()

    let mutable state = model.InitialState
    let mutable lastTime = 0.0
    let mutable externalTime = None
    let inputBuf = InputEventBufferImpl()

    override this.Initialize(initialTime) =
        lastTime <- initialTime
        this.NextTime <- initialTime + model.TimeAdvance state

    override this.CollectOutputs(time) =
        assert (time >= lastTime && time <= this.NextTime)

        if time = this.NextTime then
            model.Output state
        else
            Seq.empty

    override this.Inbox(event, time) =
        match externalTime with
        | Some x -> assert (time = x)
        | None ->
            assert (time >= lastTime && time <= this.NextTime)
            externalTime <- Some time

        inputBuf.Add(event, time)

    override this.AdvanceSimulation(time) =
        match externalTime with
        | Some x ->
            assert (time = x)
            externalTime <- None
        | None -> assert (time >= lastTime && time <= this.NextTime)

        let env = SimEnvImpl(time) :> ISimEnv
        let elapsed = { Completed = time = this.NextTime; Elapsed = time - this.NextTime }
        state <- model.Transition (state, env, elapsed, inputBuf :> IInputEventBuffer)
        lastTime <- time
        this.NextTime <- time + model.TimeAdvance state

[<Sealed>]
type CoupledModelSimulator(subcoordinators: ImmutableArray<Coordinator>,
                           translations: ImmutableArray<ImmutableArray<int * (obj -> obj option)>>,
                           inputTranslations: ImmutableArray<int * (obj -> obj option)>,
                           outputTranslations: ImmutableArray<int * (obj -> obj option)>) =
    inherit Coordinator()

    let mutable lastTime = 0.0
    let mutable externalTime = 0.0
    let inbox = List<obj>()

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

    override this.Inbox(event: obj, time: float) =
        if inbox.Count = 0 then
            assert (time <= this.NextTime)
            externalTime <- time
        else
            assert (time = externalTime)

        inbox.Add(event)

    override this.AdvanceSimulation(time) =
        assert (time >= lastTime && time <= this.NextTime)

        /// シミュレーションを進められる Coordinator のインデックス
        let synchronizeSet = HashSet()

        /// イベントを配送し、配送先モデルを synchronizeSet に追加する
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

        /// Coupled モデルに入力されたイベントを配送する
        let deliverExternalInputs () =
            let inboxEvents =
                if inbox.Count = 0 then
                    Array.empty
                else
                    assert (time = externalTime)
                    let inboxEvents = inbox.ToArray()
                    inbox.Clear()
                    inboxEvents
            deliverInputs inputTranslations inboxEvents

        /// timeAdvance が完了したモデルの出力を、接続先モデルに配送する
        let collectImminents () =
            // TODO: Future Event List の管理
            let imminentSubcoordinators =
                subcoordinators
                |> Seq.indexed
                |> Seq.filter (fun (_, c) -> c.NextTime = time)
            for (imminentIndex, coordinator) in imminentSubcoordinators do
                synchronizeSet.Add(imminentIndex) |> ignore
                deliverInputs translations.[imminentIndex] (coordinator.CollectOutputs(time))

        deliverExternalInputs ()
        collectImminents ()

        for coordinatorIndex in synchronizeSet do
            let coordinator = subcoordinators.[coordinatorIndex]
            coordinator.AdvanceSimulation(time)

        this.NextTime <- subcoordinators |> Seq.map (fun c -> c.NextTime) |> Seq.min

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
                    match RoDic.tryFind id model.Translations with
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
