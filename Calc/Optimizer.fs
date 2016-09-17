
module Optimizer

open TypeChecker
open Tokenizer
open Eval

[<AutoOpen>]
module private Optimizer = 
    let toConst (v:obj) = function
    | String -> TConstStr (v :?> string)
    | Decimal ->  (v :?> decimal) |> number.Real |> TConstNum
    | Type.Integer ->  (v :?> int) |> number.Integer |> TConstNum
    | Boolean -> TConstBool (v :?> bool)

    let isConst = function
    | TConstStr _
    | TConstNum _
    | TConstBool _ -> true
    | _ -> false

    let getValue = function
    | TConstStr s -> box s
    | TConstNum (number.Integer i) -> box i
    | TConstNum (number.Real r) -> box r
    | TConstBool b -> box b
    | _ -> failwith "Cannot get value! Something went wrong here..."

let optimizer (expr: TypedExpr) : TypedExpr =
    
    let rec optimizer' expr = 
        match expr with
        | TReference (_)
        | TConstStr _
        | TConstNum _
        | TConstBool _ -> expr
        | TFunctionCall (mi, returnType, ps) ->
            let ps = ps |> List.map optimizer'
            if ps |> Seq.forall isConst then
                try
                    let v = 
                        match mi.IsStatic, ps with
                        | false, this :: ps ->
                            mi.Invoke(this, ps |> List.map getValue |> Array.ofList)
                        | _ , ps -> 
                            mi.Invoke(null, ps |> List.map getValue |> Array.ofList)
                    toConst v returnType
                with :? System.Reflection.TargetInvocationException ->
                    //TODO: Handle it somewhow - currently we will crash during runtime...
                    TFunctionCall(mi, returnType, ps)
            else
                TFunctionCall(mi, returnType, ps)
        | TNegate expr ->
            match optimizer' expr with
            | TConstNum (number.Integer x) -> number.Integer -x |> TConstNum 
            | TConstNum (number.Real x) -> number.Real -x |> TConstNum
            | TConstBool x -> not x |> TConstBool
            | _ -> TNegate(expr)
        | TOperatorCall (op, lhs, rhs:TypedExpr, opType) ->
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
                |> fun v -> toConst v opType
            else
                TOperatorCall (op, optimizer' lhs, optimizer' rhs, opType)
        
        | TConvertType (currentType, newType, expr:TypedExpr) ->
            let expr = optimizer' expr
            match currentType, newType with
            | _, String ->
                match expr with
                | TConstStr _ as x -> x
                | TConstBool x -> x.ToString() |> TConstStr
                | TConstNum x ->
                    match x with
                    | number.Integer x -> x.ToString() |> TConstStr
                    | number.Real x -> x.ToString() |> TConstStr
                | _ -> TConvertType (currentType, newType, expr)
            | Type.Integer, Decimal ->
                match expr with
                | TConstNum (number.Integer x) -> decimal x |> (number.Real >> TConstNum)
                | _ -> TConvertType (currentType, newType, expr)
            | _ -> TConvertType (currentType, newType, expr)

    optimizer' expr
