namespace PopDEVS

open System
open System.Collections.Immutable
open System.Threading

type ComponentId internal (_v: int64) = struct end

[<Struct>]
type ComponentReference<'I, 'O> internal (id: ComponentId) =
    member __.Id = id

module ComponentId =
    let mutable value = 0L
    let create () = ComponentId(Interlocked.Increment(&value))

type ElapsedTime =
    { Completed: bool; Elapsed: float }

module internal BoxedDevsModel =
    type AtomicModel =
        { Name: string option;
          Transition: obj * ElapsedTime * obj seq -> obj;
          TimeAdvance: obj -> float;
          Output: obj -> obj seq;
          InitialState: obj }

    and CoupledModel =
        { Name: string option;
          Components: ImmutableDictionary<ComponentId, Model>;
          Translations: ImmutableDictionary<ComponentId, ImmutableDictionary<ComponentId, obj -> obj option>>;
          InputTranslations: ImmutableDictionary<ComponentId, obj -> obj option>;
          OutputTranslations: ImmutableDictionary<ComponentId, obj -> obj option> }

    and Model =
        | Atomic of AtomicModel
        | Coupled of CoupledModel

[<AbstractClass>]
type DevsModel internal (model: BoxedDevsModel.Model) =
    member __.Name =
        match model with
        | BoxedDevsModel.Model.Atomic x -> x.Name
        | BoxedDevsModel.Model.Coupled x -> x.Name

    member internal __.Inner = model

    override this.ToString() =
        let modelType =
            match model with
            | BoxedDevsModel.Model.Atomic _ -> "AtomicModel"
            | BoxedDevsModel.Model.Coupled _ -> "CoupledModel"

        match this.Name with
        | Some name -> sprintf "%s <%s>" modelType name
        | None -> modelType

[<AbstractClass>]
type DevsModel<'I, 'O> internal (model) =
    inherit DevsModel(model)

type AtomicModel<'I, 'O> internal (model) =
    inherit DevsModel<'I, 'O>(model)

type CoupledModel<'I, 'O> internal (model) =
    inherit DevsModel<'I, 'O>(model)

    static let empty =
        let model: BoxedDevsModel.CoupledModel =
            { Name = None;
              Components = ImmutableDictionary.Empty;
              Translations = ImmutableDictionary.Empty;
              InputTranslations = ImmutableDictionary.Empty;
              OutputTranslations = ImmutableDictionary.Empty }
        CoupledModel(BoxedDevsModel.Model.Coupled model)
    static member Empty = empty

module DevsModel =
    let (|Atomic|Coupled|) (model: DevsModel) =
        match model.Inner with
        | BoxedDevsModel.Model.Atomic _ -> Atomic
        | BoxedDevsModel.Model.Coupled _ -> Coupled

module AtomicModel =
    let create (transition: 'S * ElapsedTime * InputEventBag<'I> -> 'S,
                timeAdvance: 'S -> float,
                output: 'S -> 'O seq)
               (initialState: 'S) =
        let transition (s, e, i) =
            let inputBag = InputEventBag(ImmutableArray.CreateRange(Seq.map unbox i))
            transition (unbox s, e, inputBag) |> box

        let timeAdvance s =
            let ta = unbox s |> timeAdvance
            if ta < 0.0 then
                raise (InvalidOperationException("timeAdvance returned a negative number."))
            ta

        let output s = unbox s |> output |> Seq.map box

        let model: BoxedDevsModel.AtomicModel =
            { Name = None;
              Transition = transition;
              TimeAdvance = timeAdvance;
              Output = output;
              InitialState = box initialState }
        AtomicModel<'I, 'O>(BoxedDevsModel.Model.Atomic model)

    let withName name (model: AtomicModel<'I, 'O>) =
        let inner =
            match model.Inner with
            | BoxedDevsModel.Model.Atomic x ->
                BoxedDevsModel.Model.Atomic { x with Name = Some name }
            | _ -> raise (ArgumentException("model is not a valid AtomicModel"))
        AtomicModel<'I, 'O>(inner)

module CoupledModel =
    let private getInner (model: CoupledModel<'I, 'O>) =
        match model.Inner with
        | BoxedDevsModel.Model.Coupled x -> x
        | _ -> raise (ArgumentException("The speficied model is not a valid CoupledModel"))

    let private updateModel f (model: CoupledModel<'I, 'O>) =
        let updatedInner = f (getInner model)
        CoupledModel<'I, 'O>(BoxedDevsModel.Model.Coupled updatedInner)

    let withName name model =
        updateModel (fun x -> { x with Name = Some name }) model

    /// <summary>コンポーネントを追加します。</summary>
    /// <returns>更新された <see cref="CoupledModel{I,O}"/> と、 <paramref name="submodel"/> への参照を表す <see cref="ComponentReference{I,O}"/> のタプル。</returns>
    let addComponent (submodel: DevsModel<'a, 'b>) (coupledModel: CoupledModel<'c, 'd>) =
        let id = ComponentId.create ()
        let updateInner (x: BoxedDevsModel.CoupledModel) =
            { x with Components = x.Components.Add(id, submodel.Inner) }
        updateModel updateInner coupledModel, ComponentReference<'a, 'b>(id)

    let private validateComponent (paramName: string) target (coupledModel: CoupledModel<_, _>) =
        let inner = getInner coupledModel
        if not (inner.Components.ContainsKey(target)) then
            raise (ArgumentException("The specified ComponentReference is not a component of the coupled model.", paramName))

    let private boxedTransFunc transFunc event = transFunc (unbox event) |> Option.map box

    /// <summary>コンポーネント間のイベントの変換規則を追加します。</summary>
    let addTranslation (src: ComponentReference<_, 'a>, dst: ComponentReference<'b, _>)
                       (transFunc: 'a -> 'b option)
                       (model: CoupledModel<_, _>) =
        let src, dst = src.Id, dst.Id
        validateComponent "src" src model
        validateComponent "dst" dst model

        let updateInner (inner: BoxedDevsModel.CoupledModel) =            
            let transFunc = boxedTransFunc transFunc
            let translations =
                let translationsByFrom = inner.Translations
                let translationsByTo =
                    translationsByFrom.TryFind(src)
                    |> Option.defaultValue ImmutableDictionary.Empty
                let translationsByTo = translationsByTo.SetItem(dst, transFunc)
                translationsByFrom.SetItem(src, translationsByTo)
            { inner with Translations = translations }

        updateModel updateInner model

    /// <summary>入力イベントに対する変換規則を追加します。</summary>
    let addInputTranslation (dst: ComponentReference<'b, _>)
                            (transFunc: 'a -> 'b option)
                            (model: CoupledModel<'a, _>) =
        let dst = dst.Id
        validateComponent "dst" dst model

        let updateInner (inner: BoxedDevsModel.CoupledModel) =
            let transFunc = boxedTransFunc transFunc
            { inner with
                InputTranslations = inner.InputTranslations.SetItem(dst, transFunc) }

        updateModel updateInner model

    /// <summary>出力イベントに対する変換規則を追加します。</summary>
    let addOutputTranslation (src: ComponentReference<_, 'a>)
                             (transFunc: 'a -> 'b option)
                             (model: CoupledModel<_, 'b>) =
        let src = src.Id
        validateComponent "src" src model

        let updateInner (inner: BoxedDevsModel.CoupledModel) =
            let transFunc = boxedTransFunc transFunc
            { inner with
                OutputTranslations = inner.OutputTranslations.SetItem(src, transFunc) }

        updateModel updateInner model

    let countComponents (model: CoupledModel<_, _>) =
        (getInner model).Components.Count

[<AbstractClass; Sealed>]
type NoInput () = class end
