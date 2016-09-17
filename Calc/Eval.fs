module Eval

open TypeChecker
open Calc.Lib
open Tokenizer

    let (|EInt|EStr|EDecimal|EBool|) (a:obj) = 
        match a with
        | :? int as a -> EInt a
        | :? string as a -> EStr a
        | :? decimal as a -> EDecimal a
        | :? bool as a -> EBool a
        | _ -> failwithf "unsupported type %A" (a.GetType())
            
    let add = function
        |EInt a, EInt b -> (a + b) |>box
        |EStr a, EStr b -> (a + b) |>box
        |EDecimal a, EDecimal b -> (a + b) |>box
        |EBool a, EBool b -> (a && b) |>box

    let sub = function
        |EInt a, EInt b -> (a - b) |>box
        |EDecimal a, EDecimal b -> (a - b) |>box
            
    let mul = function
        |EInt a, EInt b -> (a * b) |>box
        |EDecimal a, EDecimal b -> (a * b) |>box
        |EBool a, EBool b -> (a && b) |>box
            
    let div = function
        |EInt a, EInt b -> (a / b) |>box
        |EDecimal a, EDecimal b -> (a / b) |>box

    let eq = function
        |EInt a, EInt b -> (a = b) |>box
        |EStr a, EStr b -> (a = b) |>box
        |EDecimal a, EDecimal b -> (a = b) |>box
        |EBool a, EBool b -> (a = b) |>box
            
    let ineq = function
        |EInt a, EInt b -> (a <> b) |>box
        |EStr a, EStr b -> (a <> b) |>box
        |EDecimal a, EDecimal b -> (a <> b) |>box
        |EBool a, EBool b -> (a <> b) |>box

            
    let lt = function
        |EInt a, EInt b -> (a < b) |>box
        |EStr a, EStr b -> (a < b) |>box
        |EDecimal a, EDecimal b -> (a < b) |>box
        |EBool a, EBool b -> (a < b) |>box

    let gt = function
        |EInt a, EInt b -> (a > b) |>box
        |EStr a, EStr b -> (a > b) |>box
        |EDecimal a, EDecimal b -> (a > b) |>box
        |EBool a, EBool b -> (a > b) |>box

    let lte = function
        |EInt a, EInt b -> (a <= b) |>box
        |EStr a, EStr b -> (a <= b) |>box
        |EDecimal a, EDecimal b -> (a <= b) |>box
        |EBool a, EBool b -> (a <= b) |>box

    let gte = function
        |EInt a, EInt b -> (a >= b) |>box
        |EStr a, EStr b -> (a >= b) |>box
        |EDecimal a, EDecimal b -> (a >= b) |>box
        |EBool a, EBool b -> (a >= b) |>box

    let negate = function
        |EInt a -> -a |>box
        |EDecimal a -> -a |>box
        |EBool a -> (not a) |>box



    let eval (expr:TypedExpr) (accessor:IReferenceAccessor) =
        let rec eval' = function
            | TConstNum (number.Integer i) -> box i
            | TConstNum (number.Real i) -> box i
            | TConstStr str -> box str
            | TConstBool b -> box b
            | TOperatorCall (op, lhs, rhs, _) ->
                let lhs = eval' lhs
                let rhs = eval' rhs
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
                |> fun f->f(lhs, rhs)
            | TNegate e -> eval' e |> negate
            | TReference(name, t) ->
                match t with
                | String -> accessor.GetString name |> box
                | Boolean -> accessor.GetBoolean name |> box
                | Decimal -> accessor.GetDecimal name |> box
                | Type.Integer -> accessor.GetInt name |> box
            | TConvertType (currentType, toType, e) ->
                let v = eval' e
                match currentType, toType with
                | _, String -> v.ToString() |> box
                | Type.Integer, Decimal -> v :?> int |> decimal |> box
            | TFunctionCall (mi, _, es) ->
                let es = es |> List.map eval'
                try
                    match mi.IsStatic, es with
                    | false, this::params' -> mi.Invoke(this, params' |> Array.ofList)
                    | _, params' -> mi.Invoke(null, params' |> Array.ofList)
                with :? System.Reflection.TargetInvocationException  as e 
                    -> raise e.InnerException
        try
            eval' expr |> Some
        with 
        | :? System.FormatException
        | :? System.OverflowException 
        | :? System.DivideByZeroException 
            -> None 
