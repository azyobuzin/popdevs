namespace PopDEVS

open System
open System.Collections.Generic
open System.Collections.Immutable

type internal ComponentId = int

type CoupledModel<'TInput, 'TOutput> internal
    (components: ImmutableDictionary<DevsModel, ComponentId>,
     translations: ImmutableDictionary<DevsModel, ImmutableDictionary<DevsModel, obj -> obj option>>,
     inputTranslations: ImmutableDictionary<DevsModel, obj -> obj option>,
     outputTranslations: ImmutableDictionary<DevsModel, obj -> obj option>) =
    inherit DevsModel<'TInput, 'TOutput>()

    // ImmutableDictionary.Keys は yield return で実装されているので、
    // Count で件数を先に取得しておくことでカバー
    let keysList (dic: IReadOnlyDictionary<_, _>) =
        let keys = List(dic.Count)
        keys.AddRange(dic.Keys)
        keys

    let componentKeys = lazy (keysList components :> IReadOnlyCollection<_>)
    member __.Components = componentKeys.Force()

    member internal __.ComponentsInternal = components
    member internal __.Translations = translations
    member internal __.InputTranslations = inputTranslations
    member internal __.OutputTranslations = outputTranslations

    (*
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
    *)

module CoupledModel =
    let create (components: DevsModel seq) =
        let componentsBuilder = ImmutableDictionary.CreateBuilder()
        for c in components do
            if not (componentsBuilder.ContainsKey(c)) then
                // Count を ID とする
                componentsBuilder.Add(c, componentsBuilder.Count)

        CoupledModel(componentsBuilder.ToImmutable(), ImmutableDictionary.Empty, ImmutableDictionary.Empty, ImmutableDictionary.Empty)

    let addComponent ``component`` (model: CoupledModel<_, _>) =
        let prevComponents = model.ComponentsInternal
        if prevComponents.ContainsKey(``component``) then
            model
        else
            let newComponents = prevComponents.Add(``component``, prevComponents.Count)
            CoupledModel(newComponents, model.Translations, model.InputTranslations, model.OutputTranslations)

    let addComponents (components: DevsModel seq) (model: CoupledModel<_, _>) =
        let componentsBuilder = model.ComponentsInternal.ToBuilder()
        let mutable modified = false
        for c in components do
            if not (componentsBuilder.ContainsKey(c)) then
                componentsBuilder.Add(c, componentsBuilder.Count)
                modified <- true

        if modified then CoupledModel(componentsBuilder.ToImmutable(), model.Translations, model.InputTranslations, model.OutputTranslations)
        else model

    let private ensureModelIsComponent (paramName: string) target (coupledModel: CoupledModel<_, _>) =
        if not (coupledModel.ComponentsInternal.ContainsKey(target)) then
            raise (ArgumentException("The specified component model is not a component of the coupled model.", paramName))

    let private boxedTransFunc transFunc event = transFunc (unbox event) |> Option.map box

    /// <summary>コンポーネント間のイベントの変換規則を追加します。</summary>
    let translation (fromComponent: DevsModel<_, 'a>, toComponent: DevsModel<'b, _>) (transFunc: 'a -> 'b option) (model: CoupledModel<_, _>) =
        ensureModelIsComponent "fromComponent" fromComponent model
        ensureModelIsComponent "toComponent" toComponent model

        let transFunc = boxedTransFunc transFunc

        let translations =
            let translationsByFrom = model.Translations
            let translationsByTo =
                translationsByFrom.TryFind(fromComponent)
                |> Option.defaultValue ImmutableDictionary.Empty
            let translationsByTo = translationsByTo.SetItem(toComponent, transFunc)
            translationsByFrom.SetItem(fromComponent, translationsByTo)

        CoupledModel(model.ComponentsInternal, translations, model.InputTranslations, model.OutputTranslations)

    /// <summary>入力イベントに対する変換規則を追加します。</summary>
    let inputTranslation (toComponent: DevsModel<'b, _>) (transFunc: 'a -> 'b option) (model: CoupledModel<'a, _>) =
        ensureModelIsComponent "toComponent" toComponent model

        let transFunc = boxedTransFunc transFunc
        let translations = model.InputTranslations.SetItem(toComponent, transFunc)

        CoupledModel(model.ComponentsInternal, model.Translations, translations, model.OutputTranslations)

    /// <summary>出力イベントに対する変換規則を追加します。</summary>
    let outputTranslation (fromComponent: DevsModel<_, 'a>) (transFunc: 'a -> 'b option) (model: CoupledModel<_, 'b>) =
        ensureModelIsComponent "fromComponent" fromComponent model

        let transFunc = boxedTransFunc transFunc
        let translations = model.OutputTranslations.SetItem(fromComponent, transFunc)

        CoupledModel(model.ComponentsInternal, model.Translations, model.InputTranslations, translations)
