namespace PopDEVS.ProcessOriented

open System.Collections.Immutable
open FSharp.Quotations
open FSharp.Quotations.Evaluator
open FSharp.Quotations.ExprShape
open MutableCfg
open PopDEVS
    
[<AutoOpen>]
module ProcessModelBuilder =
    let processModel<'I> = ProcessModelBuilderImpl.Builder<'I>()

type ProcessModel<'I, 'O> = ProcessEnv<'I, 'O> -> ProcessModelBuilderResult<'I>

module ProcessModel =
    type private ProcessModelState<'O> =
        { StateIndex: int
          WaitCondition: WaitConditionInner option
          TimeAdvance: float
          Outputs: 'O list }

    let private createAtomicModelFromCompiledStates<'I, 'O> (processEnv: ProcessEnv<'I, 'O>)
                                                            (states: ImmutableArray<CompiledState>)
                                                            : AtomicModel<'I, 'O> =
        let transition (state, env, elapsed, inputBuf: InputEventBuffer<'I>) =
            let canTransition =
                elapsed.Completed || // すべての出力を送信した
                List.isEmpty state.Outputs // 出力待ちはない

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
                        transitionLoop (nextState, newWaitCondition, outputs @ newOutputs)

                    | Some _ ->
                        // 終了状態
                        stateIndex, None, outputs

                    | None ->
                        // 条件を満たしていないので、待機する必要がある
                        stateIndex, waitCondition, outputs

                let nextState, waitCondition, outputs =
                    transitionLoop (state.StateIndex, state.WaitCondition, [])

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
            if List.isEmpty state.Outputs then
                state.TimeAdvance
            else
                // ただちにメッセージを出力したい
                0.0

        let output state = state.Outputs :> seq<'O>

        let initialState =
            { StateIndex = 0
              WaitCondition = None
              TimeAdvance = 0.0
              Outputs = [] }

        AtomicModel.create (transition, timeAdvance, output) initialState

    /// `node.Expr` をコンパイルし、 `variables -> waitResult -> (int * WaitCondition option)` の関数を返す
    let private compileNode varConvTable (node: ImmutableNode) =
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
                | Patterns.VarSet (v, e) as x ->
                    match indexExpr v with
                    | Some idxExpr ->
                        let assignValueExpr =
                            FsExpr.Coerce(convVar e, typeof<obj>)
                            |> FsExpr.Cast<obj>
                        <@@ (%varsExpr).[%idxExpr] <- %assignValueExpr @@>
                    | None -> x
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
                    node.LambdaParameter,
                    convVar node.Expr))
            |> FsExpr.Cast<obj[] -> obj -> int * WaitCondition option>

        expr.Evaluate()

    let createAtomicModel<'I, 'O> (processModel: ProcessModel<'I, 'O>) =
        let processEnv = ProcessEnv<'I, 'O>()

        let cfg =
            let builderResult = processModel processEnv
            StateReducer.reduceGraph builderResult.ControlFlowGraph

        let varsArray =
            cfg.Variables
            |> Seq.map (fun var -> Option.defaultValue Unchecked.defaultof<obj> var.CapturedValue)
            |> Seq.toArray

        let compile =
            let varConvTable =
                cfg.Variables
                |> Seq.indexed
                |> Seq.map (fun (i, v) -> v.FsVar, i)
                |> Map.ofSeq
            let compileNode = compileNode varConvTable
            System.Func<_, _>(fun n ->
                { Transition = compileNode n <| varsArray
                  Edges = n.Edges })

        let states = ImmutableArray.CreateRange(cfg.Nodes, compile)

        createAtomicModelFromCompiledStates processEnv states
