namespace PopDEVS.ProcessOriented

open System.Collections.Immutable
open PopDEVS

[<AutoOpen>]
module ProcessModelBuilder =
    let processModel<'I> = ProcessModelBuilderImpl.Builder<'I>()
    
module ProcessModel =
    type private ProcessModelState<'O> =
        { StateIndex: int
          Variables: ImmutableArray<obj>
          TimeAdvance: float
          Outputs: 'O list }

    let private createAtomicModelFromCompiledStates (states: ImmutableArray<CompiledState>, varlen) =
        let transition (state, _env, _elapsed, _inputBuf) =
            // TODO: env, inputBuf を ProcessEnv に反映する

            if List.isEmpty state.Outputs then
                if state.StateIndex >= 0 then
                    // TODO: 待機中の WaitCondition があるならばチェックする
                    let waitResult = Unchecked.defaultof<obj>

                    // 状態遷移を行う
                    let compiledState = states.[state.StateIndex]
                    let vars = Array.ofSeq state.Variables
                    let edge, waitCondition = compiledState.Transition (waitResult, vars)

                    let nextState =
                        if edge >= 0 && edge < compiledState.Edges.Length then
                            compiledState.Edges.[edge]
                        else
                            -1 // 範囲外の遷移は、シミュレーション終了を表す
                    
                    { StateIndex = nextState
                      Variables = ImmutableArray.CreateRange(vars)
                      TimeAdvance = 0.0 // TODO: waitCondition から TimeAdvance を引っ張り出す
                      Outputs = [] // TODO: ProcessEnv から出力を収集する
                    }
                else
                    // 終了状態
                    state
            else
                // 出力すべきメッセージがあるので、何もしない
                state

        let timeAdvance state =
            if List.isEmpty state.Outputs then
                state.TimeAdvance
            else
                // ただちにメッセージを出力したい
                0.0

        let output state = state.Outputs :> seq<_>

        let initialState =
            let vars = ImmutableArray.CreateBuilder(varlen)
            vars.Count <- varlen // デフォルト値を詰める
            { StateIndex = 0
              Variables = vars.MoveToImmutable()
              TimeAdvance = 0.0
              Outputs = [] }

        AtomicModel.create (transition, timeAdvance, output) initialState

    let createAtomicModel<'I, 'O> (processModel: ProcessEnv<'I, 'O> -> ProcessModelBuilderResult<'I>) =
        raise (System.NotImplementedException()) :> AtomicModel<'I, 'O>
