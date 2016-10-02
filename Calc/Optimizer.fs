
module Optimizer

open TypeChecker
open Tokenizer
open Eval

[<AutoOpen>]
module private Optimizer = 
    let toConst e (v:obj) =
        match e with
        | String -> TStr (v :?> string)
        | Decimal ->  (v :?> decimal)|> TDecimal
        | Type.Integer ->  (v :?> int) |> TInteger
        | Boolean -> TBool (v :?> bool)
        | DateTime -> TDateTime (v :?> System.DateTime)
        | Date -> TDate (v :?> Calc.Lib.Date)
        |> TConst

    let isConst = function
    | TConst _ -> true
    | _ -> false

    let getValue = function
    | TConst c ->
        match c with
        | TStr s -> box s
        | TInteger i -> box i
        | TDecimal r -> box r
        | TBool b -> box b
        | TDate b -> box b
        | TDateTime b -> box b
        | _ -> failwith "Cannot get value! Something went wrong here..."
    | _ -> failwith "Cannot get value! Something went wrong here..."

let optimizer (expr: TypedExpr) : TypedExpr =
    
    let rec optimizer' expr = 
        match expr with
        | TReference (_)
        | TConst _ -> expr
        | TFunctionCall (_, UserDefined _, _, _) -> expr
        | TFunctionCall (mi, returnType, ps, referntionalTransparent) ->
            let ps = ps |> List.map optimizer'
            if referntionalTransparent && ps |> Seq.forall isConst then
                try
                    match mi.IsStatic, ps with
                    | false, this :: ps ->
                        mi.Invoke(this, ps |> List.map getValue |> Array.ofList)
                    | _ , ps -> 
                        mi.Invoke(null, ps |> List.map getValue |> Array.ofList)
                    |> toConst returnType
                with :? System.Reflection.TargetInvocationException ->
                    //TODO: Handle it somewhow - currently we will crash during runtime...
                    TFunctionCall(mi, returnType, ps, referntionalTransparent)
            else
                TFunctionCall(mi, returnType, ps, referntionalTransparent)
        | TNegate expr ->
            match optimizer' expr with
            | TConst (TInteger x) -> TInteger -x |> TConst 
            | TConst (TDecimal x) -> TDecimal -x |> TConst
            | TConst (TBool x) -> not x |> (TBool >> TConst)
            | _ -> TNegate(expr) 
        | TOperatorCall (_,_,_, UserDefined _) -> expr
        | TOperatorCall (op, lhs, rhs, opType) ->
            let lhs = optimizer' lhs
            let rhs = optimizer' rhs
            if isConst lhs && isConst rhs then
                match op with
                | Concat 
                | Plus -> add
                | Minus -> sub
                | Multiply -> mul
                | Divide -> div
                | Equals -> eq
                | Inequality -> ineq
                | Greater -> gt
                | GreaterOrEqual -> gte
                | Less -> lt
                | LessOrEqual -> lte
                |> fun f->f(getValue lhs, getValue rhs)
                |> fun v -> toConst opType v
            else
                TOperatorCall (op, lhs, rhs, opType)
        
        | TConvertType (currentType, newType, expr:TypedExpr) ->
            let expr = optimizer' expr
            match currentType, newType with
            | _, String ->
                match expr with
                | TConst c ->
                    match c with
                    | TStr x -> x
                    | TDate x -> x.ToString()
                    | TDateTime s -> s.ToString()
                    | TBool x -> x.ToString()
                    | TInteger x -> x.ToString()
                    | TDecimal x -> x.ToString()
                    |> (TStr>>TConst)
                | _ -> TConvertType (currentType, newType, expr)
            | Type.Integer, Decimal ->
                match expr with
                | TConst (TInteger x) -> decimal x |> (TDecimal >> TConst)
                | _ -> TConvertType (currentType, newType, expr)
            | Type.Date, DateTime ->
                match expr with
                | TConst (TDate d) -> System.DateTime(d.Year, d.Month, d.Day) |> (TDateTime>>TConst)
                | _ -> TConvertType (currentType, newType, expr)
            | _ -> TConvertType (currentType, newType, expr)
        | TCallCtor (ctor, params') ->
            try
                let vals = params' |> List.map optimizer'
                if List.forall isConst vals then
                    ctor.Invoke (vals |> List.map getValue |> Array.ofList)
                    |> toConst (Type.ToType ctor.DeclaringType)
                else expr
            with _ -> expr //TODO: fix this temporarty hack.
    optimizer' expr
