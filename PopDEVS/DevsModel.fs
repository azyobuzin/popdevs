namespace PopDEVS

open System
open System.Collections.Immutable

[<AbstractClass>]
type DevsModel() = class end

[<AbstractClass>]
type DevsModel<'TInput, 'TOutput>() =
    inherit DevsModel()

[<AbstractClass>]
type AtomicModel<'TState, 'TInput, 'TOutput>() =
    inherit DevsModel<'TInput, 'TOutput>()

    abstract member InternalTransition : state: 'TState -> 'TState

    abstract member TimeAdvance : state: 'TState -> float

    abstract member Output : state: 'TState -> 'TOutput list
    default __.Output(_state) = []

    abstract member ExternalTransition : state: 'TState * inputs: 'TInput list -> 'TState
    default __.ExternalTransition(state, _inputs) = state

    abstract member ConfluentTransition : state: 'TState * inputs: 'TInput list -> 'TState
    default __.ConfluentTransition(state, _inputs) = state

type CoupledModel<'TInput, 'TOutput>
    (components: ImmutableArray<DevsModel>,
     translations: Map<int, (int * (obj -> obj option)) list>,
     inputTranslations: (int * ('TInput -> obj option)) list,
     outputTranslations: Map<int, obj -> 'TOutput option>) =
    inherit DevsModel<'TInput, 'TOutput>()

    let validComponent i = i >= 0 && i < components.Length

    member __.Components = components

    member __.Translate(src: int, event: obj) =
        if validComponent src |> not then
            raise (ArgumentOutOfRangeException("src"))

        let applyTranslation (dst, translate) =
            translate event |> Option.map (fun x -> dst, x)

        Map.tryFind src translations
        |> Option.defaultValue []
        |> List.choose applyTranslation

    member __.TranslateInput(src: int, event: 'TInput) =
        if validComponent src |> not then
            raise (ArgumentOutOfRangeException("src"))

        let applyTranslation (dst, translate) =
            translate event |> Option.map (fun x -> dst, x)

        inputTranslations
        |> List.choose applyTranslation

    member __.TranslateOutput(src: int, event: obj) =
        if validComponent src |> not then
            raise (ArgumentOutOfRangeException("src"))

        Map.tryFind src outputTranslations
        |> Option.bind (fun f -> f event)

// TODO: AtomicModel インスタンス自身が状態を表した方が筋が良さそう & ReferenceEquals
// TODO: CoupledModel をいい感じに作っていくやつ
