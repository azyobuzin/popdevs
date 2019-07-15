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

type [<CustomEquality; NoComparison>] internal BoxedAtomicModel =
    { Name: string option;
      Transition: obj * ElapsedTime * obj seq -> obj;
      TimeAdvance: obj -> float;
      Output: obj -> obj seq;
      InitialState: obj }

    override this.Equals(other) =
        obj.ReferenceEquals(this, other) ||
        match other with
        | :? BoxedAtomicModel as x ->
            this.Name = x.Name &&
            (Unchecked.equals this.Transition x.Transition) &&
            (Unchecked.equals this.TimeAdvance x.TimeAdvance) &&
            (Unchecked.equals this.Output x.Output) &&
            this.InitialState = x.InitialState
        | _ -> false

    override this.GetHashCode() =
        ((hash this.Name) * 397) ^^^
        ((Unchecked.hash this.Transition) * 397) ^^^
        ((Unchecked.hash this.TimeAdvance) * 397) ^^^
        ((Unchecked.hash this.Output) * 397) ^^^
        (hash this.InitialState)

and internal BoxedCoupledModel =
    { Name: string option;
      Components: ImmutableDictionary<ComponentId, DevsModel>;
      Translations: ImmutableDictionary<ComponentId, ImmutableDictionary<ComponentId, obj -> obj option>>;
      InputTranslations: ImmutableDictionary<ComponentId, obj -> obj option>;
      OutputTranslations: ImmutableDictionary<ComponentId, obj -> obj option> }

and [<StructuralEquality; NoComparison>] internal BoxedModel =
    | Atomic of BoxedAtomicModel
    | Coupled of BoxedCoupledModel

and [<AbstractClass>] DevsModel internal (model: BoxedModel) =
    member __.Name =
        match model with
        | BoxedModel.Atomic x -> x.Name
        | BoxedModel.Coupled x -> x.Name

    member internal __.Inner = model

    override this.ToString() =
        let modelType =
            match model with
            | BoxedModel.Atomic _ -> "AtomicModel"
            | BoxedModel.Coupled _ -> "CoupledModel"

        match this.Name with
        | Some name -> sprintf "%s <%s>" modelType name
        | None -> modelType

    override this.Equals(other) =
        obj.ReferenceEquals(this, other) ||
        match other with
        | :? DevsModel as x -> model.Equals(x.Inner)
        | _ -> false

    override __.GetHashCode() =
        model.GetHashCode()

[<AbstractClass>]
type DevsModel<'I, 'O> internal (model) =
    inherit DevsModel(model)

module DevsModel =
    let (|Atomic|Coupled|) (model: DevsModel) =
        match model.Inner with
        | BoxedModel.Atomic _ -> Atomic
        | BoxedModel.Coupled _ -> Coupled

type ComponentReference<'I, 'O> internal (id: ComponentId, model: DevsModel<'I, 'O>) =
    member __.Id = id
    member __.Model = model

    override this.Equals(other) =
        obj.ReferenceEquals(this, other) ||
        match other with
        | :? ComponentReference<'I, 'O> as x -> id = x.Id
        | _ -> false

    override __.GetHashCode() =
        id.GetHashCode()

    override __.ToString() =
        sprintf "%O -> %O" id model

[<AbstractClass; Sealed>]
type VoidEvent() = class end
