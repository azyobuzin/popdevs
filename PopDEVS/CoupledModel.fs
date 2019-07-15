namespace PopDEVS

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Linq

type CoupledModel<'I, 'O> internal (model) =
    inherit DevsModel<'I, 'O>(model)

    static member val Empty =
        let model: BoxedDevsModel.CoupledModel =
            { Name = None;
              Components = ImmutableDictionary.Empty;
              Translations = ImmutableDictionary.Empty;
              InputTranslations = ImmutableDictionary.Empty;
              OutputTranslations = ImmutableDictionary.Empty }
        CoupledModel(BoxedDevsModel.Model.Coupled model)

type CoupledModelBuilder<'I, 'O> internal
        (name, components: ImmutableDictionary.Builder<_, _>,
         translations: Dictionary<_, ImmutableDictionary.Builder<_, _>>,
         inputTranslations, outputTranslations) =

    let validateComponent (paramName: string) target =
        if not (components.ContainsKey(target)) then
            raise (ArgumentException("The specified ComponentReference is not a component of the coupled model.", paramName))

    let boxedTransFunc transFunc event = transFunc (unbox event) |> Option.map box

    new() =
        CoupledModelBuilder(
            None,
            ImmutableDictionary.CreateBuilder(),
            Dictionary(),
            ImmutableDictionary.CreateBuilder(),
            ImmutableDictionary.CreateBuilder())

    member val Name : string option = name with get, set

    member __.ComponentCount = components.Count

    /// <summary>コンポーネントを追加します。</summary>
    /// <returns><paramref name="submodel"/> への参照を表す <see cref="ComponentReference{I,O}"/>。</returns>
    member __.AddComponent(submodel: DevsModel<'a, 'b>) =
        let id = ComponentId.Create()
        components.Add(id, submodel.Inner)
        ComponentReference(id, submodel)

    /// <summary>コンポーネント間のイベントの変換規則を追加します。</summary>
    member __.Connect(src: ComponentReference<_, 'a>, dst: ComponentReference<'b, _>, transFunc: 'a -> 'b option) =
        let src, dst = src.Id, dst.Id
        validateComponent "src" src
        validateComponent "dst" dst

        let transFunc = boxedTransFunc transFunc

        match translations.TryFind(src) with
        | Some x -> x.[dst] <- transFunc
        | None ->
            let builder = ImmutableDictionary.CreateBuilder()
            builder.Add(dst, transFunc)
            translations.Add(src, builder)

    /// <summary>入力イベントに対する変換規則を追加します。</summary>
    member __.AddInputTranslation(dst: ComponentReference<'a, _>, transFunc: 'I -> 'a option) =
        let dst = dst.Id
        validateComponent "dst" dst

        inputTranslations.[dst] <- boxedTransFunc transFunc

    /// <summary>出力イベントに対する変換規則を追加します。</summary>
    member __.AddOutputTranslation(src: ComponentReference<_, 'a>, transFunc: 'a -> 'O option) =
        let src = src.Id
        validateComponent "src" src

        outputTranslations.[src] <- boxedTransFunc transFunc

    member this.Build() =
        let translations =
            translations.ToImmutableDictionary(
                (fun kvp -> kvp.Key),
                (fun kvp -> kvp.Value.ToImmutable()))
        let model: BoxedDevsModel.CoupledModel =
            { Name = this.Name;
              Components = components.ToImmutable();
              Translations = translations;
              InputTranslations = inputTranslations.ToImmutable();
              OutputTranslations = outputTranslations.ToImmutable() }
        CoupledModel<'I, 'O>(BoxedDevsModel.Model.Coupled model)

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

    let countComponents (model: CoupledModel<_, _>) =
        (getInner model).Components.Count

    let toBuilder (model: CoupledModel<'I, 'O>) =
        let inner = getInner model
        CoupledModelBuilder<'I, 'O>(
            inner.Name,
            inner.Components.ToBuilder(),
            inner.Translations.ToDictionary(
                (fun kvp -> kvp.Key),
                (fun kvp -> kvp.Value.ToBuilder())),
            inner.InputTranslations.ToBuilder(),
            inner.OutputTranslations.ToBuilder())
