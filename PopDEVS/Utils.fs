namespace PopDEVS

open System.Collections.Generic
open System.Runtime.CompilerServices

[<AutoOpen>]
module internal Utils =
    let boolToOption b = if b then Some () else None

    let mapToList mapping (source: IReadOnlyCollection<_>) =
        let count = source.Count
        let list = List(count)
        for x in source do list.Add(mapping x)
        list

[<Extension>]
type internal CollectionExtensions =
    [<Extension>]
    static member TryFind(dic: IReadOnlyDictionary<'a, 'b>, key) =
        let mutable v = Unchecked.defaultof<'b>
        if dic.TryGetValue(key, &v) then Some v else None
