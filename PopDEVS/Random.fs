module PopDEVS.Random

open FsRandom

let nextRef generator stateRef =
    let r, s = Random.next generator !stateRef
    stateRef := s
    r

let nextByref generator (stateByref: byref<_>) =
    let r, s = Random.next generator stateByref
    stateByref <- s
    r
