module internal PopDEVS.ProcessOriented.ProcessGraphBuilder

open System
open System.Collections.Generic
open FSharp.Quotations
open MutableCfg

type private CfgEnv =
    { /// 外部からキャプチャした変数
      CapturedVariables: Dictionary<string, MutableVar>
      /// コンピュテーション式内で定義された変数
      Variables: Dictionary<FsVar, MutableVar> }

[<RequireQualifiedAccess>]
type private Tree =
    | Expr of FsExpr * cont: Tree
    | Let of FsVar * cont: Tree
    | If of t: Tree * f: Tree * cont: Tree
    | While of cond: Tree * body: Tree * cont: Tree
    | Bind of cont: Tree
    | Zero

module private BuilderPatterns =
    let (|CallBind|_|) builder = function
        | Patterns.Call (Some (Patterns.Value (receiver, _)), method, [computation; binder])
                when obj.Equals(receiver, builder) && method.Name = "Bind" ->
            match binder with
            | Patterns.Lambda (var, body) -> Some (computation, var, body)
            | _ -> failwith "The second argument of the Bind call is not a lambda."
        | _ -> None

    let (|CallCombine|_|) builder = function
        | Patterns.Call (Some (Patterns.Value (receiver, _)), method, [left; right])
                when obj.Equals(receiver, builder) && method.Name = "Combine" ->
            Some (left, right)
        | _ -> None

    let (|CallWhile|_|) builder = function
        | Patterns.Call (Some (Patterns.Value (receiver, _)), method, [guard; computation])
                when obj.Equals(receiver, builder) && method.Name = "While" ->
            match guard with
            | Patterns.Lambda (_, cond) -> Some (cond, computation)
            | _ -> failwith "The first argument of the While call is not a lambda."
        | _ -> None

    let (|CallZero|_|) builder = function
        | Patterns.Call (Some (Patterns.Value (receiver, _)), method, [])
                when obj.Equals(receiver, builder) && method.Name = "Zero" ->
            Some ()
        | _ -> None

    let (|CallDelay|_|) builder = function
        | Patterns.Call (Some (Patterns.Value (receiver, _)), method, [generator])
                when obj.Equals(receiver, builder) && method.Name = "Delay" ->
            match generator with
            | Patterns.Lambda (_, body) -> Some body
            | _ -> failwith "The argument of the Delay call is not a lambda."
        | _ -> None

let rec private continueWith cont tree =
    let rec f = function
        | Tree.Expr (x, Tree.Zero) ->
            match cont with
            | Tree.Expr (y, z) -> Tree.Expr (FsExpr.Sequential(x, y), z)
            | _ -> Tree.Expr (x, cont)
        | Tree.Expr (x, y) -> Tree.Expr (x, f y)
        | Tree.Let (x, y) -> Tree.Let (x, f y)
        | Tree.If (x, y, z) -> Tree.If (x, y, f z)
        | Tree.While (x, y, z) -> Tree.While (x, y, f z)
        | Tree.Bind (x, y) -> Tree.Bind (x, f y)
        | Tree.Zero -> cont
    f tree

let rec private rebuildExpr tree =
    let rec rebuild tree stack =
        match tree with
        | Tree.Expr (expr, cont) -> rebuild cont (expr::stack)
        | Tree.Let (var, cont) ->
            let expr = stackToExpr stack
            match rebuild cont [] with
            | Some body -> Some (FsExpr.Let(var, expr, body))
            | None -> None
        | Tree.If (t, f, cont) ->
            match rebuildExpr t, rebuildExpr f with
            | Some t, Some f ->
                let condExpr = stackToExpr stack
                rebuild cont [FsExpr.IfThenElse(condExpr, t, f)]
            | _ -> None
        | Tree.While (cond, body, cont) ->
            match rebuildExpr cond, rebuildExpr body with
            | Some cond, Some body -> rebuild cont [FsExpr.WhileLoop(cond, body)]
            | _ -> None
        | Tree.Bind _ -> None
        | Tree.Zero -> Some (stackToExpr stack)
    and stackToExpr = function
        | [] -> <@@ () @@>
        | [expr] -> expr
        | expr::stack -> FsExpr.Sequential(stackToExpr stack, expr)
    rebuild tree []

let toTree (input: ProcessModelBuilderResult<'I>) =
    let builder = input.Builder
    let capturedVars = Dictionary<string, FsVar * obj>()

    let recordCapturedVar (value, varType, name) =
        if isNull varType then nullArg (nameof varType)
        if String.IsNullOrEmpty(name) then invalidArg (nameof name) "name is null or empty."

        match RoDic.tryFind name capturedVars with
        | Some (var, existingValue) ->
            // すでに記録されているので、アサーション
            if not (obj.Equals(value, existingValue)) then
                failwithf "Value mismatch (Expected: %O, Actual: %O)" existingValue value

            var
        | None ->
            // 新規追加
            let var = FsVar(name, varType)
            env.CapturedVariables.Add(name, (fsVar, value))
            var

    let rec toTreeCore = function
        | Patterns.Let (var, expr, cont) ->
            toTreeCore expr
            |> continueWith (Tree.Let (var, toTreeCore cont)) // TODO: let if needed

        | Patterns.IfThenElse (guard, thenExpr, elseExpr) ->
            let guardTree = toTreeCore guard
            let thenTree = toTreeCore thenExpr
            let elseTree = toTreeCore elseExpr
            let tree = guardTree |> continueWith (Tree.If (thenTree, elseTree, Tree.Zero))
            match rebuildExpr tree with
                | Some expr -> Tree.Expr (expr, Tree.Zero)
                | None -> tree

        | BuilderPatterns.CallWhile builder (guard, body) ->
            let guardTree = toTreeCore guard
            let bodyTree = toTreeCore OneWayBody
            let tree = Tree.While (guardTree, bodyTree, Tree.Zero)
            match rebuildExpr tree with
                | Some expr -> Tree.Expr (expr, Tree.Zero)
                | None -> tree

        | BuilderPatterns.CallBind builder (expr, var, cont) ->
            toTreeCore expr
            |> continueWith (Tree.Bind (Tree.Let (var, toTreeCore cont))) // TODO: let if needed

        | BuilderPatterns.CallZero builder | DerivedPatterns.Unit ->
            Tree.Zero

        | BuilderPatterns.CallCombine builder (left, right)
        | Patterns.Sequential (left, right) ->
            toTreeCore left |> continueWith (toTreeCore right)

        | BuilderPatterns.CallDelay builder x ->
            toTreeCore x



let build (input: ProcessModelBuilderResult<'I>) =
    let builder = input.Builder
    let env = newEnv ()
    
    /// 出現した ValueWithName を env に記録する
    let recordCapturedVar (value, varType, name) =
        if isNull varType then nullArg (nameof varType)
        if String.IsNullOrEmpty(name) then invalidArg (nameof name) "name is null or empty."
    
        match RoDic.tryFind name env.CapturedVariables with
        | Some var ->
            // すでに記録されているので、アサーション
            if not (obj.Equals(varType, var.FsVar.Type)) then
                failwithf "Type mismatch (Expected: %O, Actual: %O)" var.FsVar.Type varType
    
            let existingValue = var.CapturedValue.Value
            if not (obj.Equals(value, existingValue)) then
                failwithf "Value mismatch (Expected: %O, Actual: %O)" existingValue value
    
            var
        | None ->
            // 新規追加
            let fsVar = FsVar(name, varType)
            let newVar = { FsVar = fsVar
                            CapturedValue = Some value
                            IsEscaped = false }
            env.CapturedVariables.Add(name, newVar)
            env.Variables.Add(fsVar, newVar)
            newVar
    
    /// let された変数を env に追加する
    let addVar (var: FsVar) =
        let x = { FsVar = var
                    CapturedValue = None
                    IsEscaped = false }
        env.Variables.Add(var, x)
    
    let markAsEscaped (var: FsVar) =
        match RoDic.tryFind var env.Variables with
        | Some x -> x.IsEscaped <- true
        | None -> ()

    let rec hasBind = function
        | BuilderPatterns.CallBind builder _ -> true
        | ExprShape.ShapeCombination (_shape, args) ->
            List.exists hasBind args
        | _ -> false

    let rec createNode (expr: FsExpr, kind: TailKind) =
        match createNodeOrExpr (expr, kind) with
        | Node nodes -> nodes
        | Expr expr ->
            raise (NotImplementedException())

    and createNodeOrExpr (expr, kind) =
        match expr with
        | _ when hasBind expr -> Expr expr
