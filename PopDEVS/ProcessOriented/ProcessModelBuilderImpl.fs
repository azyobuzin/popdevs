module PopDEVS.ProcessOriented.ProcessModelBuilderImpl

open System
open System.Collections.Immutable
open FSharp.Quotations

type internal FsExpr = FSharp.Quotations.Expr
type internal FsExpr<'T> = FSharp.Quotations.Expr<'T>
type internal FsVar = FSharp.Quotations.Var

type Placeholder<'I, 'O, 'V> = struct end

(*
[<ReferenceEquality>]
type internal StateVar =
    { Type: Type
      /// 外部からキャプチャした変数なら、その値を代入
      CapturedValue: obj option
      /// ラムダ式にキャプチャされる変数か
      mutable IsEscaped: bool
      mutable ReferenceCount: int
      mutable Index: int option }

type internal ControlFlowNode =
    { /// 処理を行い、次に遷移する辺のインデックスを返す
      Expr: FsExpr<int>
      Edges: ImmutableArray<CfgNode> }

and internal EventNode =
    { WaitConditionExpr: FsExpr // -> WaitCondition<'I, 'O, 'a>
      ContinuationExpr: FsExpr // 'a -> int
      Edges: ImmutableArray<CfgNode> }

and [<ReferenceEquality>] internal CfgNode =
    | Block of ControlFlowNode
    | Event of EventNode
    | Exit
*)

type internal BuilderResultInner =
    | Expr of FsExpr
    | Bind of FsExpr * BuilderResultInner
    | Combine of BuilderResultInner * BuilderResultInner
    | While of FsExpr * BuilderResultInner

type BuilderResult<'I, 'O> internal (inner: BuilderResultInner) =
    member internal __.Inner = inner

type Builder<'I, 'O>() =
    let doNotCall () =
        invalidOp "Do not call methods of Builder from your code."

    member __.Bind<'a, 'b>(_computation: Placeholder<'I, 'O, WaitCondition<'I, 'a>>,
                           _binder: 'a -> Placeholder<'I, 'O, 'b>)
                           : Placeholder<'I, 'O, 'b> =
        doNotCall ()

    member __.Combine<'a>(_left: Placeholder<'I, 'O, unit>,
                          _right: Placeholder<'I, 'O, 'a>)
                          : Placeholder<'I, 'O, 'a> =
        doNotCall ()

    member __.While(_guard: unit -> bool, _computation: Placeholder<'I, 'O, unit>)
                    : Placeholder<'I, 'O, unit> =
        doNotCall ()

    member __.Zero() : Placeholder<'I, 'O, unit> =
        doNotCall ()

    member __.Delay<'a>(_: unit -> Placeholder<'I, 'O, 'a>)
                        : Placeholder<'I, 'O, 'a> =
        doNotCall ()

    member __.Quote(_: Expr<Placeholder<'I, 'O, unit>>)
                    : Expr<Placeholder<'I, 'O, unit>> =
        doNotCall ()

    member this.Run(expr: Expr<Placeholder<'I, 'O, unit>>) =
        printfn "%O" expr

        let isLambda x =
            match x with
            | Patterns.Lambda _ -> true
            | _ -> false

        let rec toResult = function
            | DerivedPatterns.SpecificCall <@@ this.Delay @@>
                (_, _, [generator]) ->
                match generator with
                | Patterns.Lambda (_, body) -> toResult body
                | x -> failwith "The argument of the Delay call is not a lambda."
            | DerivedPatterns.SpecificCall <@@ this.Bind @@>
                (_, _, [arg1; arg2]) ->
                if not (isLambda arg2) then
                    failwith "The second argument of the Bind call is not a lambda."
                BuilderResultInner.Bind (arg1, toResult arg2)
            | DerivedPatterns.SpecificCall <@@ this.Combine @@>
                (_, _, [arg1; arg2]) ->
                BuilderResultInner.Combine (toResult arg1, toResult arg2)
            | DerivedPatterns.SpecificCall <@@ this.While @@>
                (_, _, [arg1; arg2]) ->
                if not (isLambda arg1) then
                    failwith "The first argument of the While call is not a lambda."
                BuilderResultInner.While (arg1, toResult arg2)
            | Patterns.Call (Some (Patterns.Value (receiver, _)), method, _)
                when obj.ReferenceEquals(receiver, this) ->
                failwithf "Do not call '%s'." method.Name
            | Patterns.Sequential (left, right) as expr ->
                let left = toResult left
                let right = toResult right
                match left, right with
                | BuilderResultInner.Expr _, BuilderResultInner.Expr _ ->
                    BuilderResultInner.Expr expr
                | left, right -> BuilderResultInner.Combine (left, right)
            | expr -> BuilderResultInner.Expr expr

        BuilderResult<'I, 'O>(toResult expr)
