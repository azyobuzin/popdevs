module PopDEVS.ProcessOriented.ProcessModelBuilder

open System
open FSharp.Quotations

type Placeholder<'I, 'O, 'V> = struct end

[<ReferenceEquality>]
type internal StateVar =
    { Type: Type
      /// 外部からキャプチャした変数か
      IsCaptured: bool
      /// ラムダ式にキャプチャされる変数か
      mutable IsEscaped: bool
      mutable ReferenceCount: int
      mutable Index: int option }

// TODO: CFG

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
        raise (System.NotImplementedException())
