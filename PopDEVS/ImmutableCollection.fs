namespace PopDEVS

open System.Collections.Generic
open System.Runtime.CompilerServices

[<Extension>]
type internal ImmutableCollectionExtensions =
    [<Extension>]
    static member TryFind(dic: IReadOnlyDictionary<'a, 'b>, key) =
        let mutable v = Unchecked.defaultof<'b>
        if dic.TryGetValue(key, &v) then Some v else None
