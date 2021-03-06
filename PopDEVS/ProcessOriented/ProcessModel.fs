namespace PopDEVS.ProcessOriented

open System.Collections.Immutable
open FSharp.Quotations
open FSharp.Quotations.Evaluator
open FSharp.Quotations.ExprShape
open FSharpx.Collections
open PgUtils
open PopDEVS
    
[<AutoOpen>]
module ProcessModelBuilders =
    type ProcessModelBuilder<'I>() =
        let doNotCall () =
            invalidOp "Do not call methods of Builder from your code directly."
    
        member __.Bind<'a, 'b>(_computation: WaitCondition<'I, 'a>, _binder: 'a -> 'b) : 'b =
            doNotCall ()
    
        member __.Combine<'a>(_left: unit, _right: 'a) : 'a =
            doNotCall ()
    
        member __.While(_guard: unit -> bool, _computation: unit) : unit =
            doNotCall ()
    
        member __.Zero() : unit =
            doNotCall ()
    
        member __.Delay<'a>(_: unit -> 'a) : 'a =
            doNotCall ()
    
        member __.Quote(_: Expr<unit>) : Expr<unit> =
            doNotCall ()

        member this.Run(expr: Expr<unit>) =
            ProcessModelBuilderResult<'I>(this, expr)

    let processModel<'I> = ProcessModelBuilder<'I>()

type ProcessModel<'I, 'O> = ProcessEnv<'I, 'O> -> ProcessModelBuilderResult<'I>

module ProcessModel =
    type private ProcessModelState<'O> =
        { StateIndex: int
          WaitCondition: WaitConditionInner option
          TimeAdvance: float
          Outputs: DList<'O> }

    let private createAtomicModelFromCompiledStates<'I, 'O> (processEnv: ProcessEnv<'I, 'O>)
                                                            (states: ImmutableArray<CompiledState>)
                                                            : AtomicModel<'I, 'O> =
        let transition (state, env, elapsed, inputBuf: InputEventBuffer<'I>) =
            let canTransition =
                elapsed.Completed || // すべての出力を送信した
                DList.isEmpty state.Outputs // 出力待ちはない

            if canTransition then
                let rec transitionLoop (stateIndex, waitCondition: WaitConditionInner option, outputs) =
                    let waitResultOption =
                        match waitCondition with
                        | Some cond ->
                            cond.Poll(env, box inputBuf)
                            cond.Result
                        | None ->
                            // 待機を行っていない場合は、 null を戻り値ということにしておく
                            Some Unchecked.defaultof<obj>

                    match waitResultOption with
                    | Some waitResult when stateIndex >= 0 ->
                        // ProcessEnv を用意する
                        processEnv.SetSimEnv(env)

                        // 状態遷移を行う
                        let compiledState = states.[stateIndex]
                        let edge, newWaitCondition = compiledState.Transition waitResult

                        let nextState =
                            if edge >= 0 && edge < compiledState.Edges.Length then
                                compiledState.Edges.[edge]
                            else
                                -1 // 範囲外の遷移は、シミュレーション終了を表す
                        let newWaitCondition = newWaitCondition |> Option.map (fun x -> x.Inner)

                        // ProcessEnv から出力を取得する
                        let newOutputs = processEnv.Reset()

                        // さらに遷移できるかを試す
                        transitionLoop (nextState, newWaitCondition, DList.append outputs newOutputs)

                    | Some _ ->
                        // 終了状態
                        stateIndex, None, outputs

                    | None ->
                        // 条件を満たしていないので、待機する必要がある
                        stateIndex, waitCondition, outputs

                let nextState, waitCondition, outputs =
                    transitionLoop (state.StateIndex, state.WaitCondition, DList.empty)

                let timeAdvance =
                    match waitCondition with
                    | Some x -> x.TimeAdvance(SimEnv.getTime env)
                    | None -> if nextState >= 0 then 0.0 else infinity

                { state with
                    StateIndex = nextState
                    WaitCondition = waitCondition
                    TimeAdvance = timeAdvance
                    Outputs = outputs }
            else
                // 出力すべきメッセージがあるので、何もしない
                state

        let timeAdvance state =
            if DList.isEmpty state.Outputs then
                state.TimeAdvance
            else
                // ただちにメッセージを出力したい
                0.0

        let output state = state.Outputs :> seq<'O>

        let initialState =
            { StateIndex = 0
              WaitCondition = None
              TimeAdvance = 0.0
              Outputs = DList.empty }

        AtomicModel.create (transition, timeAdvance, output) initialState

    /// `node.Expr` をコンパイルし、 `variables -> waitResult -> (int * WaitCondition option)` の関数を返す
    let private compileNode varConvTable (node: ImmutableNode) =
        let dummyParameter = FsVar("_", typeof<obj>)

        let expr =
            /// 変数から、実際に格納する配列の添え字を求める
            let indexExpr var =
                varConvTable
                |> Map.tryFind var
                |> Option.map (FsExpr.Value<int> >> FsExpr.Cast<int>)

            let varsParam = FsVar("variables", typeof<obj[]>, false)
            let varsExpr = FsExpr.Cast<obj[]>(FsExpr.Var(varsParam))

            /// 変数の読み書きを、配列の読み書きに書き換える
            let rec convVar = function
                | Patterns.VarSet (v, e) ->
                    match indexExpr v with
                    | Some idxExpr ->
                        let assignValueExpr =
                            FsExpr.Coerce(convVar e, typeof<obj>)
                            |> FsExpr.Cast<obj>
                        <@@ (%varsExpr).[%idxExpr] <- %assignValueExpr @@>
                    | None -> FsExpr.VarSet(v, convVar e)
                | ShapeVar v ->
                    match indexExpr v with
                    | Some idxExpr -> <@@ (%varsExpr).[%idxExpr] @@> |> unboxExpr v.Type
                    | None -> FsExpr.Var(v)
                | ShapeLambda (v, e) ->
                    FsExpr.Lambda(v, convVar e)
                | ShapeCombination (shape, es) ->
                    RebuildShapeCombination(shape, List.map convVar es)

            FsExpr.Lambda(
                varsParam,
                FsExpr.Lambda(
                    Option.defaultValue dummyParameter node.LambdaParameter,
                    convVar node.Expr))
            |> FsExpr.Cast<obj[] -> obj -> int * WaitCondition option>

        #if DEBUG
        // 未定義の変数が残っていないことを確認
        let freeVars = expr.GetFreeVars() |> List.ofSeq
        if not (List.isEmpty freeVars) then
            failwithf "Free vars: %O" freeVars
        #endif

        expr.Evaluate()

    let createAtomicModel<'I, 'O> (processModel: ProcessModel<'I, 'O>) =
        let processEnv = ProcessEnv<'I, 'O>()

        let graph = processModel processEnv |> ProcessGraphBuilder.build

        let varsArray =
            graph.Variables
            |> Seq.map (fun var -> Option.defaultValue Unchecked.defaultof<obj> var.CapturedValue)
            |> Seq.toArray

        let compile =
            let varConvTable =
                graph.Variables
                |> Seq.indexed
                |> Seq.map (fun (i, v) -> v.FsVar, i)
                |> Map.ofSeq
            let compileNode = compileNode varConvTable
            System.Func<_, _>(fun n ->
                { Transition = compileNode n <| varsArray
                  Edges = n.Edges })

        let states = ImmutableArray.CreateRange(graph.Nodes, compile)

        createAtomicModelFromCompiledStates processEnv states
