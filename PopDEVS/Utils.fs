namespace PopDEVS

open System.Collections.Generic

[<AutoOpen>]
module internal Utils =
    let boolToOption b = if b then Some () else None

    let mapToList mapping (source: IReadOnlyCollection<_>) =
        let count = source.Count
        let list = List(count)
        for x in source do list.Add(mapping x)
        list

    let mapToArray mapping (source: IReadOnlyCollection<_>) =
        let count = source.Count
        let arr = Array.zeroCreate count
        for i, x in Seq.indexed source do
            arr.[i] <- mapping x
        arr

module internal RoDic =
    let tryFind key (dic: IReadOnlyDictionary<_, _>) =
        match dic.TryGetValue(key) with
        | true, x -> Some x
        | _ -> None
