module internal PopDEVS.ProcessOriented.LetRecUtils

open FSharp.Quotations

type LetRecKind =
    | NotRecursive
    | Recursive
    | MutuallyRecursive

let bindingsWithKind (bindings: (Var * Expr) list) =
    let varSet = bindings |> Seq.map fst |> set
    let k self =
        let otherVars = varSet |> Set.remove self
        let rec exprKind = function
            | ExprShape.ShapeVar v ->
                if otherVars |> Set.contains v then MutuallyRecursive
                elif v = self then Recursive
                else NotRecursive
            | ExprShape.ShapeLambda (_, x) -> exprKind x
            | ExprShape.ShapeCombination (_, exprs) ->
                let rec kmany = function
                    | e :: es ->
                        let x = exprKind e
                        if x = MutuallyRecursive then MutuallyRecursive
                        else
                            let y = kmany es
                            match y with
                            | Recursive | MutuallyRecursive -> y
                            | NotRecursive -> x
                    | [] -> NotRecursive
                kmany exprs
        exprKind
    bindings |> List.map (fun ((v, e) as t) -> t, k v e)
