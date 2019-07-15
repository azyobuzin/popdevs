namespace PopDEVS

open System.Collections.Immutable
open System.Threading

[<Struct>]
type ComponentId internal (v: int64) =
    static let mutable value = 0L
    static member Create() = ComponentId(Interlocked.Increment(&value))

    override __.ToString() =
        v.ToString()

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

module DevsModel =
    let (|Atomic|Coupled|) (model: DevsModel) =
        match model.Inner with
        | BoxedDevsModel.Model.Atomic _ -> Atomic
        | BoxedDevsModel.Model.Coupled _ -> Coupled

[<Struct; CustomEquality; NoComparison>]
type ComponentReference<'I, 'O> internal (id: ComponentId, model: DevsModel<'I, 'O>) =
    member __.Id = id
    member __.Model = model

    override __.Equals(other) =
        match other with
        | :? ComponentReference<'I, 'O> as x -> id = x.Id
        | _ -> false

    override __.GetHashCode() =
        id.GetHashCode()

    override __.ToString() =
        sprintf "%O -> %O" id model

[<AbstractClass; Sealed>]
type NoInput () = class end
