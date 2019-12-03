module PopDEVS.EventObserver

type ObservedEvent<'a> =
    { ComponentId: ComponentId;
      Model: DevsModel;
      Time: float;
      Event: 'a }

type ObserverModel<'a> = AtomicModel<ObservedEvent<'a>, VoidEvent>
type ObserverReference<'a> = ComponentReference<ObservedEvent<'a>, VoidEvent>

/// <summary>イベントが入力されると <paramref name="onEvent"/> を呼び出す <see cref="AtomicModel{I,O}"/> を作成します。</summary>
let createObserverModel (onEvent: ObservedEvent<'a> -> unit) : ObserverModel<'a> =
    let transition ((), env, _, inputBuf) =
        let fire () =
            inputBuf
            |> InputEventBuffer.take (fun re -> Some {
                re.Event with ObservedEvent.Time = re.Time })
            |> Seq.iter onEvent
        SimEnv.runIO fire env
    let timeAdvance () = infinity
    let output () = Seq.empty<VoidEvent>
    AtomicModel.create (transition, timeAdvance, output) ()

/// <summary>指定したコンポーネントを <see cref="ObserverModel{a}"/> に接続します。</summary>
let connect (coupledModel: CoupledModelBuilder<_, _>)
            (observee: ComponentReference<_, 'a>)
            (observer: ObserverReference<'a>) =
    coupledModel.Connect(
        observee, observer,
        (fun event -> Some {
            ComponentId = observee.Id;
            Model = observee.Model;
            Time = 0.0;
            Event = event }))

/// <summary>指定したコンポーネントを <see cref="ObserverModel{a}"/> に接続します。</summary>
let connectBoxed (coupledModel: CoupledModelBuilder<_, _>)
                 (observee: ComponentId)
                 (observer: ObserverReference<obj>) =
    let model =
        match RoDic.tryFind observee coupledModel.Components with
        | Some x -> x
        | None -> invalidArg (nameof observee) "observee is not a component of the coupled model."
    coupledModel.Connect(
        observee, observer,
        (fun event -> Some {
            ComponentId = observee;
            Model = model;
            Time = 0.0;
            Event = event }))

/// <summary><paramref name="coupledModel"/> のすべてのコンポーネントを <paramref name="observer"/> に接続します。</summary>
let connectAll (coupledModel: CoupledModelBuilder<_, _>)
               (observer: ObserverReference<obj>) =
    coupledModel.Components
    |> Seq.filter (fun kvp -> kvp.Key <> observer.Id)
    |> Seq.iter (fun kvp ->
        coupledModel.Connect(
            kvp.Key, observer,
            (fun event -> Some {
                ComponentId = kvp.Key;
                Model = kvp.Value;
                Time = 0.0;
                Event = event })))

/// <summary>指定したコンポーネントの出力を観測する <see cref="ObserverModel{a}"/> を作成し、接続します。</summary>
/// <returns>作成された <see cref="ObserverModel{a}"/> と、 <paramref name="coupledModel"/> における参照のタプル。</returns>
let observe (onEvent: ObservedEvent<'a> -> unit)
            (coupledModel: CoupledModelBuilder<_, _>)
            (observee: ComponentReference<_, 'a>) =
    let observerModel = createObserverModel onEvent
    let observerReference : ObserverReference<'a> =
        coupledModel.AddComponent(observerModel)
    connect coupledModel observee observerReference
    observerModel, observerReference

/// <summary>指定したコンポーネントの出力を観測する <see cref="ObserverModel{a}"/> を作成し、接続します。</summary>
/// <returns>作成された <see cref="ObserverModel{a}"/> と、 <paramref name="coupledModel"/> における参照のタプル。</returns>
let observeBoxed (onEvent: ObservedEvent<obj> -> unit)
                 (coupledModel: CoupledModelBuilder<_, _>)
                 (observee: ComponentId) =
    let observerModel = createObserverModel onEvent
    let observerReference : ObserverReference<obj> =
        coupledModel.AddComponent(observerModel)
    connectBoxed coupledModel observee observerReference
    observerModel, observerReference
    
/// <summary><paramref name="coupledModel"/> のすべてのコンポーネントの出力を観測する <see cref="ObserverModel{a}"/> を作成し、接続します。</summary>
/// <returns>作成された <see cref="ObserverModel{a}"/> と、 <paramref name="coupledModel"/> における参照のタプル。</returns>
let observeAll (onEvent: ObservedEvent<obj> -> unit)
               (coupledModel: CoupledModelBuilder<_, _>) =
    let observerModel = createObserverModel onEvent
    let observerReference : ObserverReference<obj> =
        coupledModel.AddComponent(observerModel)
    connectAll coupledModel observerReference
    observerModel, observerReference
