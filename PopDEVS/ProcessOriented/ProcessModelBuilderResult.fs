namespace PopDEVS.ProcessOriented

type ProcessModelBuilderResult<'I> internal (builder: obj, expr: Quotations.Expr<unit>) =
    member internal _.Builder = builder
    member internal _.Expr = expr
    override _.ToString() = string expr
