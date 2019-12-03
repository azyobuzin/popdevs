namespace PopDEVS

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Linq

module internal CoupledModelHelper =
    let getInner (model: DevsModel) =
        match model.Inner with
        | BoxedModel.Coupled x -> x
        | _ -> invalidArg (nameof model) "The speficied model is not a valid CoupledModel"

type CoupledModel<'I, 'O> internal (model) =
    inherit DevsModel<'I, 'O>(model)

    member this.Components =
        (CoupledModelHelper.getInner this).Components

    member this.InfluenceesOf(``component``: ComponentId) =
        match (CoupledModelHelper.getInner this).Translations |> RoDic.tryFind ``component`` with
        | Some dic -> dic.Keys
        | None -> Seq.empty

    member this.IsConnected(src: ComponentId, dst: ComponentId) =
        match (CoupledModelHelper.getInner this).Translations |> RoDic.tryFind src with
        | Some dic -> dic.ContainsKey(dst)
        | None -> false

    member this.GetExternalInputComponents() =
        (CoupledModelHelper.getInner this).InputTranslations.Keys

    member this.GetExternalOutputComponents() =
        (CoupledModelHelper.getInner this).OutputTranslations.Keys

    [<CompiledName("Empty")>]
    static member val empty =
        let model: BoxedCoupledModel =
            { Name = None;
              Components = ImmutableDictionary.Empty;
              Translations = ImmutableDictionary.Empty;
              InputTranslations = ImmutableDictionary.Empty;
              OutputTranslations = ImmutableDictionary.Empty }
        CoupledModel(BoxedModel.Coupled model)

type CoupledModelBuilder<'I, 'O> internal
        (name, components: ImmutableDictionary.Builder<_, DevsModel>,
         translations: Dictionary<_, ImmutableDictionary.Builder<_, _>>,
         inputTranslations, outputTranslations) =

    let validateComponent (paramName: string) target =
        if not (components.ContainsKey(target)) then
            invalidArg paramName "The specified ComponentReference is not a component of the coupled model."

    let boxedTransFunc transFunc event = transFunc (unbox event) |> Option.map box

    let addTranslation (src, dst, transFunc) =
        validateComponent (nameof src) src
        validateComponent (nameof dst) dst
        if src = dst then raise (ArgumentException("src and dst are the same ID."))

        match RoDic.tryFind src translations with
        | Some x -> x.[dst] <- transFunc
        | None ->
            let builder = ImmutableDictionary.CreateBuilder()
            builder.Add(dst, transFunc)
            translations.Add(src, builder)

    new() =
        CoupledModelBuilder(
            None,
            ImmutableDictionary.CreateBuilder(),
            Dictionary(),
            ImmutableDictionary.CreateBuilder(),
            ImmutableDictionary.CreateBuilder())

    member val Name : string option = name with get, set

    member __.Components = components :> IReadOnlyDictionary<_, _>

    member __.InfluenceesOf(``component``: ComponentId) =
        match RoDic.tryFind ``component`` translations with
        | Some dic -> dic.Keys
        | None -> Seq.empty

    member __.IsConnected(src: ComponentId, dst: ComponentId) =
        match RoDic.tryFind src translations with
        | Some dic -> dic.ContainsKey(dst)
        | None -> false

    member __.GetExternalInputComponents() =
        inputTranslations.Keys

    member __.GetExternalOutputComponents() =
        outputTranslations.Keys

    /// <summary>コンポーネントを追加します。</summary>
    /// <returns><paramref name="submodel"/> への参照を表す <see cref="ComponentReference{I,O}"/>。</returns>
    member __.AddComponent(submodel: DevsModel<'a, 'b>) =
        let id = ComponentId.Create()
        components.Add(id, submodel)
        ComponentReference(id, submodel)

    /// <summary>コンポーネント間のイベントの変換規則を追加します。</summary>
    member __.Connect(src: ComponentReference<_, 'a>, dst: ComponentReference<'b, _>, transFunc: 'a -> 'b option) =
        let transFunc = boxedTransFunc transFunc
        addTranslation (src.Id, dst.Id, transFunc)

    /// <summary>コンポーネント間のイベントの変換規則を追加します。このオーバーロードでは、入力イベントの型をチェックしません。</summary>
    member __.Connect(src: ComponentId, dst: ComponentReference<'a, _>, transFunc: obj -> 'a option) =
        let transFunc event = transFunc event |> Option.map box
        addTranslation (src, dst.Id, transFunc)

    /// <summary>入力イベントに対する変換規則を追加します。</summary>
    member __.AddInputTranslation(dst: ComponentReference<'a, _>, transFunc: 'I -> 'a option) =
        let dst = dst.Id
        validateComponent (nameof dst) dst

        inputTranslations.[dst] <- boxedTransFunc transFunc

    /// <summary>出力イベントに対する変換規則を追加します。</summary>
    member __.AddOutputTranslation(src: ComponentReference<_, 'a>, transFunc: 'a -> 'O option) =
        let src = src.Id
        validateComponent (nameof src) src

        outputTranslations.[src] <- boxedTransFunc transFunc

    member this.Build() =
        let translations =
            translations.ToImmutableDictionary(
                (fun kvp -> kvp.Key),
                (fun kvp -> kvp.Value.ToImmutable()))
        let model: BoxedCoupledModel =
            { Name = this.Name;
              Components = components.ToImmutable();
              Translations = translations;
              InputTranslations = inputTranslations.ToImmutable();
              OutputTranslations = outputTranslations.ToImmutable() }
        CoupledModel<'I, 'O>(BoxedModel.Coupled model)

module CoupledModel =
    let private updateModel f (model: CoupledModel<'I, 'O>) =
        let updatedInner = f (CoupledModelHelper.getInner model)
        CoupledModel<'I, 'O>(BoxedModel.Coupled updatedInner)

    [<CompiledName("WithName")>]
    let withName name model =
        updateModel (fun x -> { x with Name = Some name }) model

    [<CompiledName("ToBuilder")>]
    let toBuilder (model: CoupledModel<'I, 'O>) =
        let inner = CoupledModelHelper.getInner model
        CoupledModelBuilder<'I, 'O>(
            inner.Name,
            inner.Components.ToBuilder(),
            inner.Translations.ToDictionary(
                (fun kvp -> kvp.Key),
                (fun kvp -> kvp.Value.ToBuilder())),
            inner.InputTranslations.ToBuilder(),
            inner.OutputTranslations.ToBuilder())
