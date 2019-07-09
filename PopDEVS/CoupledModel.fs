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

    let private invalidComponent (paramName: string) =
        raise (ArgumentException("The specified component model is not a component of the coupled model.", paramName))

    let private componentIndex ``component`` (model: CoupledModel<_, _>) =
        model.Components.IndexOf(``component``)

    /// <summary>コンポーネント間のイベントの変換規則を追加します。</summary>
    let translation (fromComponent: DevsModel<_, 'a>, toComponent: DevsModel<'b, _>) (transFunc: 'a -> 'b option) (model: CoupledModel<_, _>) =
        let fromIndex = componentIndex fromComponent model
        let toIndex = componentIndex toComponent model

        if fromIndex < 0 then invalidComponent "fromComponent"
        if toIndex < 0 then invalidComponent "toComponent"

        let (|NewList|ListAvailable|TranslationAvailable|) (translations, fromIndex, toIndex) =
            // toIndex 順にソートされていると仮定して、変換規則が存在するか検索する
            let findTranslation (xs: ImmutableArray<int * _>) =
                let indexes =
                    Seq.indexed xs
                    |> Seq.map (fun (listIndex, (toIndex, _)) -> (listIndex, toIndex))
                match Seq.tryFind (fun (_, j) -> j >= toIndex) indexes with
                | Some (i, j) when j = toIndex -> TranslationAvailable (xs, i)
                | Some (i, _) -> ListAvailable (xs, i)
                | None -> ListAvailable (xs, xs.Length)

            match Map.tryFind fromIndex translations with
            | None -> NewList
            | Some xs -> findTranslation xs
            
        let tranItem = toIndex, (fun event -> transFunc (unbox event) |> Option.map box)

        let translations =
            let tranList =
                match (model.Translations, fromIndex, toIndex) with
                | NewList -> ImmutableArray.Create(tranItem)
                | ListAvailable (xs, i) -> xs.Insert(i, tranItem)
                | TranslationAvailable (xs, i) -> xs.SetItem(i, tranItem)
            Map.add fromIndex tranList model.Translations

        CoupledModel(model.Components, translations, model.InputTranslations, model.OutputTranslations)

    /// <summary>入力イベントに対する変換規則を追加します。</summary>
    let inputTranslation (toComponent: DevsModel<'b, _>) (transFunc: 'a -> 'b option) (model: CoupledModel<'a, _>) =
        let toIndex = componentIndex toComponent model
        if toIndex < 0 then invalidComponent "toComponent"

        let transItem = toIndex, (fun event -> transFunc event |> Option.map box)

        let translations =
            let xs = model.InputTranslations
            let indexes =
                Seq.indexed xs
                |> Seq.map (fun (listIndex, (toIndex, _)) -> (listIndex, toIndex))
            // toIndex 順にソートされていると仮定して、変換規則が存在するか検索し、置き換える
            match Seq.tryFind (fun (_, j) -> j >= toIndex) indexes with
            | Some (i, j) when j = toIndex -> xs.SetItem(i, transItem)
            | Some (i, _) -> xs.Insert(i, transItem)
            | None -> xs.Add(transItem)

        CoupledModel(model.Components, model.Translations, translations, model.OutputTranslations)

    /// <summary>出力イベントに対する変換規則を追加します。</summary>
    let outputTranslation (fromComponent: DevsModel<_, 'a>) (transFunc: 'a -> 'b option) (model: CoupledModel<_, 'b>) =
        let fromIndex = componentIndex fromComponent model
        if fromIndex < 0 then invalidComponent "fromComponent"

        let transFunc event = transFunc (unbox event)
        let translations = Map.add fromIndex transFunc model.OutputTranslations
        CoupledModel(model.Components, model.Translations, model.InputTranslations, translations)
