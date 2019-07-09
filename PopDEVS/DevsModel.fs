namespace PopDEVS

[<AbstractClass>]
type DevsModel() = class end

[<AbstractClass>]
type DevsModel<'TInput, 'TOutput>() =
    inherit DevsModel()
