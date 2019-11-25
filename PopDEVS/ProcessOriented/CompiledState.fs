namespace PopDEVS.ProcessOriented

open System.Collections.Immutable

type internal CompiledState =
    { /// 状態遷移関数 `waitResult -> (index, waitCondition)`
      Transition: obj -> int * WaitCondition option
      /// 遷移先状態リスト
      Edges: ImmutableArray<int> }
