module PopDEVS.ProcessOriented.ProcessModelBuilderImpl

open System
open System.Collections.Generic
open System.Collections.Immutable
open FSharp.Quotations
open PopDEVS

type internal FsExpr = FSharp.Quotations.Expr
type internal FsExpr<'T> = FSharp.Quotations.Expr<'T>
type internal FsVar = FSharp.Quotations.Var

type Placeholder<'I, 'O, 'V> = struct end

[<ReferenceEquality>]
type internal StateVar =
    { Name: string
      Type: Type
      /// 外部からキャプチャした変数なら、その値を代入
      CapturedValue: obj option
      /// ラムダ式にキャプチャされる変数か
      mutable IsEscaped: bool
      mutable ReferenceCount: int
      mutable Index: int option }

let internal newCapturedVar (name, varType, value) =
    { Name = name
      Type = varType
      CapturedValue = Some value
      IsEscaped = false
      ReferenceCount = 0
      Index = None }

let internal newVar (name, varType) =
    { Name = name
      Type = varType
      CapturedValue = None
      IsEscaped = false
      ReferenceCount = 0
      Index = None }

[<ReferenceEquality>]
type internal CfgNode =    
    { /// 前回のイベントの戻り値を受け取って、処理を行い、次に遷移する辺のインデックスを返す式
      /// 'a -> int * WaitCondition<'I, 'O, 'b> option
      mutable Expr: FsExpr
      mutable ReturnsWaitCondition: bool
      Edges: List<CfgNode> }

/// CfgNode.Expr の戻り値の種類
type internal NodeBehavior =
    /// 何も返さない（次に遷移する辺だけ指定する）
    | Unit
    /// 値を変数に代入したあと、 Unit と同じ挙動をする
    | Assign of FsVar
    /// WaitCondition を返す
    | WaitCondition

let internal newNode (expr, returnsWaitCondition) =
    { Expr = expr
      Edges = List<CfgNode>()
      ReturnsWaitCondition = returnsWaitCondition }

type internal CfgEnv =
    { /// 外部からキャプチャした変数
      CapturedVariables: Dictionary<string, StateVar>
      /// コンピュテーション式内で定義された変数
      Variables: Dictionary<FsVar, StateVar> }

let internal newEnv () =
    { CapturedVariables = new Dictionary<string, StateVar>()
      Variables = new Dictionary<FsVar, StateVar>() }

type internal BuilderTree =
    | Expr of FsExpr
    | Bind of FsExpr * BuilderTree
    | Combine of BuilderTree * BuilderTree
    | While of FsExpr * BuilderTree

type BuilderResult<'I, 'O> internal (inner: BuilderTree) =
    member internal __.Inner = inner

type Builder<'I, 'O>() =
    let doNotCall () =
        invalidOp "Do not call methods of Builder from your code."

    member __.Bind<'a, 'b>(_computation: WaitCondition<'I, 'a>, _binder: 'a -> 'b) : 'b =
        doNotCall ()

    member __.Combine<'a>(_left: unit, _right: 'a) : 'a =
        doNotCall ()

    member __.While(_guard: unit -> bool, _computation: unit) : unit =
        doNotCall ()

    member __.Zero() : unit =
        doNotCall ()

    member __.Delay<'a>(_: unit -> 'a) : 'a =
        doNotCall ()

    member __.Quote(_: Expr<unit>) : Expr<unit> =
        doNotCall ()

    member this.Run(expr: Expr<unit>) =
        printfn "%O" expr

        let env = newEnv ()

        /// 出現した ValueWithName を env に記録する
        let recordCapturedVar (value, varType, name) =
            if isNull varType then nullArg "varType"
            if String.IsNullOrEmpty(name) then invalidArg "name" "name is null or empty."

            match env.CapturedVariables.TryFind(name) with
            | Some var ->
                // すでに記録されているので、アサーション
                if not (obj.Equals(varType, var.Type)) then
                    failwithf "Type mismatch (Expected: %O, Actual: %O)" var.Type varType

                let existingValue = var.CapturedValue.Value
                if not (obj.Equals(value, existingValue)) then
                    failwithf "Value mismatch (Expected: %O, Actual: %O)" existingValue value

            | None ->
                // 新規追加
                env.CapturedVariables.Add(name, newCapturedVar (name, varType, value))

        /// let された変数を env に追加する
        let addVar (var: FsVar) =
            env.Variables.Add(var, newVar (var.Name, var.Type))

        (*
        let isLambda x =
            match x with
            | Patterns.Lambda _ -> true
            | _ -> false

        let rec toBuilderTree = function
            | DerivedPatterns.SpecificCall <@@ this.Delay @@>
                (_, _, [generator]) ->
                match generator with
                | Patterns.Lambda (_, body) -> toBuilderTree body
                | x -> failwith "The argument of the Delay call is not a lambda."
            | DerivedPatterns.SpecificCall <@@ this.Bind @@>
                (_, _, [arg1; arg2]) ->
                if not (isLambda arg2) then
                    failwith "The second argument of the Bind call is not a lambda."
                BuilderTree.Bind (arg1, toBuilderTree arg2)
            | DerivedPatterns.SpecificCall <@@ this.Combine @@>
                (_, _, [arg1; arg2]) ->
                BuilderTree.Combine (toBuilderTree arg1, toBuilderTree arg2)
            | DerivedPatterns.SpecificCall <@@ this.While @@>
                (_, _, [arg1; arg2]) ->
                if not (isLambda arg1) then
                    failwith "The first argument of the While call is not a lambda."
                BuilderTree.While (arg1, toBuilderTree arg2)
            | Patterns.Call (Some (Patterns.Value (receiver, _)), method, _)
                when obj.ReferenceEquals(receiver, this) ->
                failwithf "Do not call '%s'." method.Name
            | Patterns.Sequential (left, right) as expr ->
                let left = toBuilderTree left
                let right = toBuilderTree right
                match left, right with
                | BuilderTree.Expr _, BuilderTree.Expr _ ->
                    BuilderTree.Expr expr
                | left, right -> BuilderTree.Combine (left, right)
            | expr -> BuilderTree.Expr expr

        let tree = toBuilderTree expr
        *)

        let connect (left, right) = left.Edges.Add(right)

        let oneWay = <@ 0, None @>

        /// 条件を実行し、 false なら 0 番目、 true なら 1 番目の辺に進むノードを作成する
        let condToNode cond =
            // fun _ -> (if cond then 1 else 0), None
            let transitionExpr = FsExpr.IfThenElse(cond, <@@ 1 @@>, <@@ 0 @@>)
            let condLambda = FsExpr.Lambda(FsVar("_", typeof<obj>), FsExpr.NewTuple([transitionExpr; <@@ None @@>]))
            newNode (condLambda, false)

        let doNothingNode () =
            newNode (FsExpr.Lambda(FsVar("_", typeof<obj>), oneWay), false)

        let ensureWaitConditionType (ty: Type) =
            if ty.FullName <> "PopDEVS.ProcessOriented.WaitCondition`2" then
                failwith "The type of the expression is not WaitCondition<'I, 'R>."

        /// <param name="varToAssign">expr の式の値を代入する変数</param>
        // TODO: CfgNode or Expr が返ってこないとうまくいかない？
        let rec createCfg (expr, kind) =
            match expr with
            // Builder のメソッド呼び出し

            | DerivedPatterns.SpecificCall <@@ this.Delay @@>
                (_, _, [generator]) ->
                match generator with
                | Patterns.Lambda (_, body) -> createCfg (body, kind)
                | _ -> failwith "The argument of the Delay call is not a lambda."

            | DerivedPatterns.SpecificCall <@@ this.Bind @@>
                (_, _, [arg1; arg2]) ->
                // WaitCondition<'Input, 'Result> の 'Result を取得
                let waitResultType = arg1.Type.GetGenericArguments().[1]

                // Bind 前のノードからの継続として、 Bind 後のノードを接続
                let leftFirst, leftLast = createCfg (arg1, WaitCondition)

                match arg2 with
                | Patterns.Lambda (var, body) ->
                    addVar var

                    let assignNode =
                        // fun waitResult ->
                        //     var <- waitResult
                        //     0, None
                        let funcParam = FsVar("waitResult", typeof<obj>)
                        let assignExpr = FsExpr.VarSet(var, FsExpr.Coerce(FsExpr.Var(funcParam), waitResultType))
                        let assignLambda = FsExpr.Lambda(funcParam, FsExpr.Sequential(assignExpr, oneWay))
                        newNode (assignLambda, false)
                    connect (leftLast, assignNode)

                    let rightFirst, rightLast = createCfg (body, kind)
                    connect (assignNode, rightFirst)

                    leftFirst, rightLast

                | _ -> failwith "The second argument of the Bind call is not a lambda."

            | DerivedPatterns.SpecificCall <@@ this.Combine @@>
                (_, _, [arg1; arg2]) ->
                let leftFirst, leftLast = createCfg (arg1, Unit)
                let rightFirst, rightLast = createCfg (arg2, kind)
                connect (leftLast, rightFirst)
                leftFirst, rightLast

            | DerivedPatterns.SpecificCall <@@ this.While @@>
                (_, _, [arg1; arg2]) ->
                match arg1 with
                | Patterns.Lambda (_, cond) ->
                    match kind with
                    | Unit -> ()
                    | _ -> failwith "NodeBehavior.Unit can only be used for While."

                    let testNode = condToNode cond
                    let lastNode = doNothingNode ()
                    let (bodyFirst, bodyLast) = createCfg (arg2, Unit)

                    connect (testNode, lastNode)
                    connect (testNode, bodyFirst)
                    connect (bodyLast, testNode)

                    testNode, lastNode

                | _ -> failwith "The first argument of the While call is not a lambda."

            | Patterns.Call (Some (Patterns.Value (receiver, _)), method, _)
                when obj.ReferenceEquals(receiver, this) ->
                failwithf "Do not call '%s'." method.Name

            // その他の文法

            | Patterns.IfThenElse (cond, trueExpr, falseExpr) ->
                let testNode = condToNode cond

                let (trueFirst, trueLast), (falseFirst, falseLast), joinNode =
                    match kind with
                    | Unit | Assign _ ->
                        // true, false それぞれで、やることをやって、最後に何もしないノードに戻ってくる
                        createCfg (trueExpr, kind), createCfg (falseExpr, kind), doNothingNode ()

                    | WaitCondition ->
                        let trueType, falseType = trueExpr.Type, falseExpr.Type
                        if not (obj.Equals(trueType, falseType)) then
                            failwithf "Type mismatch (True: %O, False: %O)" trueType falseType
                        ensureWaitConditionType trueType

                        // trueExpr, falseExpr が返す WaitCondition を変数に保存して、 joinNode でそれを返す
                        // TODO: WaitCondition を状態保存に含めるわけにはいかないので、うまく変数を使うのを回避する
                        let waitCondVar = Var("waitCondition", trueType)
                        addVar waitCondVar
                        let trueTuple = createCfg (trueExpr, Assign waitCondVar)
                        let falseTuple = createCfg (falseExpr, Assign waitCondVar)
                        let joinNode =
                            let varExpr = FsExpr.Coerce(FsExpr.Var(waitCondVar), typeof<obj>)
                            let lambda = <@@ fun _ -> 0, Some %%varExpr @@>
                            newNode (lambda, true)
                        trueTuple, falseTuple, joinNode

                connect (testNode, falseFirst)
                connect (falseLast, joinNode)
                connect (testNode, trueFirst)
                connect (trueLast, joinNode)
                testNode, joinNode

            | Patterns.Let (var, value, body) -> transLet ([(var, value)], body, kind)
            | Patterns.LetRecursive (bindings, body) -> transLet (bindings, body, kind)

            | Patterns.Sequential (left, right) ->
                let leftFirst, leftLast = createCfg (left, Unit)
                let rightFirst, rightLast = createCfg (right, kind)
                connect (leftLast, rightFirst)
                leftFirst, rightLast

            | Patterns.TryFinally _ -> raise (new NotSupportedException("TryFinally"))
            | Patterns.TryWith _ -> raise (new NotSupportedException("TryWith"))
            | Patterns.WhileLoop _ -> raise (new NotSupportedException("WhileLoop"))

            // TODO: もっとちゃんと再帰的に見る
            | expr ->
                match kind with
                | Unit ->
                    let lambda = FsExpr.Lambda(FsVar("_", typeof<obj>), FsExpr.Sequential(expr, oneWay))
                    let node = newNode (lambda, false)
                    node, node
                | Assign var ->
                    let lambda =
                        FsExpr.Lambda(
                            FsVar("_", typeof<obj>),
                            FsExpr.Sequential(
                                FsExpr.VarSet(var, expr),
                                oneWay))
                    let node = newNode (lambda, false)
                    node, node
                | WaitCondition ->
                    let lambda = <@@ fun _ -> 0, Some %%(FsExpr.Coerce(expr, typeof<obj>)) @@>
                    let node = newNode (lambda, true)
                    node, node

        and transLet (bindings, body, kind) =
            let rec bindingToNode = function
                | [(var, value)] -> createCfg (value, Assign var)
                | (var, value) :: xs ->
                    let leftFirst, leftLast = createCfg (value, Assign var)
                    let rightFirst, rightLast = bindingToNode xs
                    connect (leftLast, rightFirst)
                    leftFirst, rightLast
                | [] -> invalidArg "bindings" "bindings is empty."

            let bindingFirst, bindingLast = bindingToNode bindings
            let bodyFirst, bodyLast = createCfg (body, kind)
            connect (bindingLast, bodyFirst)
            bindingFirst, bodyLast

        let rootNode, exitNode = createCfg (expr, Unit)

        raise (NotImplementedException())
        //BuilderResult<'I, 'O>(tree)
