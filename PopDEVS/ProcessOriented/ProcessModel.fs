namespace PopDEVS.ProcessOriented

open System.Collections.Generic
open System.Collections.Immutable
open FSharp.Quotations
open FSharp.Quotations.Evaluator
open FSharp.Quotations.ExprShape
open MutableCfg
open PopDEVS
    
[<AutoOpen>]
module ProcessModelBuilder =
    let processModel<'I> = ProcessModelBuilderImpl.Builder<'I>()
    
module ProcessModel =
    type private ProcessModelState<'O> =
        { StateIndex: int
          WaitCondition: WaitConditionInner option
          TimeAdvance: float
          Outputs: 'O list }

    let private createAtomicModelFromCompiledStates<'I, 'O> (processEnv: ProcessEnv<'I, 'O> option)
                                                            (states: ImmutableArray<CompiledState>)
                                                            : AtomicModel<'I, 'O> =
        let transition (state, env, _elapsed, inputBuf) =
            if List.isEmpty state.Outputs then
                if state.StateIndex >= 0 then
                    let waitResultOption =
                        match state.WaitCondition with
                        | Some cond ->
                            cond.Poll(env, box inputBuf)
                            cond.Result
                        | None ->
                            // 待機を行っていない場合は、 null を戻り値ということにしておく
                            Some Unchecked.defaultof<obj>

                    let nextState, waitCondition, outputs =
                        match waitResultOption with
                        | Some waitResult ->
                            // ProcessEnv を用意する
                            match processEnv with
                            | Some pe -> pe.SetSimEnv(env)
                            | None -> ()

                            // 状態遷移を行う
                            let compiledState = states.[state.StateIndex]
                            let edge, waitCondition = compiledState.Transition waitResult

                            let nextState =
                                if edge >= 0 && edge < compiledState.Edges.Length then
                                    compiledState.Edges.[edge]
                                else
                                    -1 // 範囲外の遷移は、シミュレーション終了を表す
                            let waitCondition = waitCondition |> Option.map (fun x -> x.Inner)

                            // ProcessEnv から出力を取得する
                            let outputs =
                                match processEnv with
                                | Some pe -> pe.Reset()
                                | None -> []

                            nextState, waitCondition, outputs
                        | None ->
                            // 条件を満たしていないので、まだ待機
                            state.StateIndex, state.WaitCondition, []

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
            { StateIndex = 0
              WaitCondition = None
              TimeAdvance = 0.0
              Outputs = [] }

        AtomicModel.create (transition, timeAdvance, output) initialState

    /// `node.Expr` をコンパイルし、 `variables -> waitResult -> (int * WaitCondition option)` の関数を返す
    let private compileNode (varConvTable: IReadOnlyDictionary<_, _>) (node: ImmutableNode) =
        let expr =
            /// 変数から、実際に格納する配列の添え字を求める
            let indexExpr var =
                varConvTable.TryFind(var)
                |> Option.map (FsExpr.Value<int> >> FsExpr.Cast<int>)

            let varsParam = FsVar("variables", typeof<obj[]>, false)
            let varsExpr = FsExpr.Cast<obj[]>(FsExpr.Var(varsParam))

            /// 変数の読み書きを、配列の読み書きに書き換える
            let rec convVar = function
                | Patterns.VarSet (v, e) as x ->
                    match indexExpr v with
                    | Some idxExpr -> <@@ (%varsExpr).[%idxExpr] <- %%(convVar e) @@>
                    | None -> x
                | ShapeVar v ->
                    match indexExpr v with
                    | Some idxExpr -> <@@ (%varsExpr).[%idxExpr] @@>
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
            |> FsExpr.Cast<obj[] -> obj option -> int * WaitCondition option>

        expr.Evaluate()

    let createAtomicModel<'I, 'O> (processModel: ProcessEnv<'I, 'O> -> ProcessModelBuilderResult<'I>) =
        // TODO: ProcessEnv<'I, 'O> の変数を 0 または 1 個含んでいるはず。それの添え字をうまくやる
        raise (System.NotImplementedException()) :> AtomicModel<'I, 'O>
