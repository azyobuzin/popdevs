namespace PopDEVS

open System
open System.Collections.Immutable

type CoupledModel<'TInput, 'TOutput> internal
    (components: ImmutableArray<DevsModel>,
     translations: Map<int, ImmutableArray<int * (obj -> obj option)>>,
     inputTranslations: ImmutableArray<int * ('TInput -> obj option)>,
     outputTranslations: Map<int, obj -> 'TOutput option>) =
    inherit DevsModel<'TInput, 'TOutput>()

    let validComponent i = i >= 0 && i < components.Length

    new () = CoupledModel(ImmutableArray.Empty, Map.empty, ImmutableArray.Empty, Map.empty)

    member __.Components = components

    member internal __.Translations = translations

    member internal __.InputTranslations = inputTranslations

    member internal __.OutputTranslations = outputTranslations

    member __.Translate(src: int, event: obj) =
        if validComponent src |> not then
            raise (ArgumentOutOfRangeException("src"))

        let applyTranslation (dst, translate) =
            translate event |> Option.map (fun x -> dst, x)

        Map.tryFind src translations
        |> Option.map (Seq.choose applyTranslation)
        |> Option.defaultValue Seq.empty

    member __.TranslateInput(src: int, event: 'TInput) =
        if validComponent src |> not then
            raise (ArgumentOutOfRangeException("src"))

        let applyTranslation (dst, translate) =
            translate event |> Option.map (fun x -> dst, x)

        inputTranslations
        |> Seq.choose applyTranslation

    member __.TranslateOutput(src: int, event: obj) =
        if validComponent src |> not then
            raise (ArgumentOutOfRangeException("src"))

        Map.tryFind src outputTranslations
        |> Option.bind (fun f -> f event)

module CoupledModel =
    let create (components: DevsModel seq) =
        CoupledModel(ImmutableArray.CreateRange(components), Map.empty, ImmutableArray.Empty, Map.empty)

    let addComponent ``component`` (model: CoupledModel<_, _>) =
        CoupledModel(model.Components.Add(``component``), model.Translations, model.InputTranslations, model.OutputTranslations)

    let addComponents (components: DevsModel seq) (model: CoupledModel<_, _>) =
        CoupledModel(model.Components.AddRange(components), model.Translations, model.InputTranslations, model.OutputTranslations)
