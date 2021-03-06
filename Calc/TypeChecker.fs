﻿module TypeChecker
open Analyse
open Tokenizer
open Core
open System.Reflection
open Calc.Lib

type Type =
| String
| Decimal
| Integer
| Boolean
| Date
| DateTime
| UserDefined of System.Type

with member t.GetBCLType =
        match t with
        | String -> typeof<string>
        | Decimal -> typeof<decimal>
        | Integer -> typeof<int>
        | Boolean -> typeof<bool>
        | Date -> typeof<Date>
        | DateTime -> typeof<System.DateTime>
        | UserDefined t -> t
     static member ToType t =
        if typeof<System.String> = t then String
        elif typeof<System.Decimal> = t then Decimal
        elif typeof<System.Boolean> = t then Boolean
        elif typeof<System.Int32> = t then Integer
        elif typeof<System.DateTime> = t then DateTime
        elif typeof<Date> = t then Date
        else UserDefined t
type TConst =
| TStr of string
| TInteger of int
| TDecimal of decimal
| TBool of bool
| TDate of Date
| TDateTime of System.DateTime
with
    member c.Type = 
        match c with
        | TStr _ -> String
        | TInteger _ -> Integer
        | TDecimal _ -> Decimal
        | TBool _ -> Boolean
        | TDate _ -> Date
        | TDateTime _ -> DateTime

type TypedExpr = 
| TConst of TConst
| TCallCtor of ctorInfo:ConstructorInfo * TypedExpr list
| TFunctionCall of methodInfo:MethodInfo * returnType:Type * TypedExpr list * referntionalTransparent:bool
| TNegate of TypedExpr
| TOperatorCall of operator * lhs:TypedExpr * rhs:TypedExpr * opType : Type
| TReference of name:RefName * refType:Type
| TConvertType of currentType: Type * newType: Type * expr:TypedExpr
with 
    member expr.Type = 
        match expr with
        | TConst x -> x.Type
        | TOperatorCall (opType = t)
        | TConvertType (newType = t) 
        | TReference (refType = t)
        | TFunctionCall (returnType = t) -> t
        | TCallCtor (ctorInfo, _) -> UserDefined ctorInfo.DeclaringType
        | TNegate expr
            -> expr.Type 
type FunName = string

type RefDef =
    { Name : RefName
      Type : Type }

type FunDef = 
    { Name : FunName
      MethodInfo : System.Reflection.MethodInfo
      IsReferntionalTransparent : bool }
with
    member def.ReturnType = def.MethodInfo.ReturnType |> FunDef.GetType
    member def.Parameters = 
        def.MethodInfo.GetParameters ()
        |> Array.map (fun p -> p.ParameterType |> FunDef.GetType)
        |> List.ofArray
    static member GetType t = Type.ToType t
        
let rec areCompatibleTypes actual expected = 
    match actual, expected with
    | a, b when a=b -> true
    | Integer, Decimal
    | Date, DateTime
    | _, String
        -> true
    | Integer, UserDefined x
    | UserDefined x, Decimal when x = typeof<Rational>
        -> true
    | _ -> false

let (|AreTypesCompatible|NotCompatibleTypes|) (expected, expr:TypedExpr) = 
    let actual = expr.Type
    if actual = expected then AreTypesCompatible expr 
    elif areCompatibleTypes actual expected then
         AreTypesCompatible(TConvertType (actual, expected, expr))
    else NotCompatibleTypes (actual, expected)

let (|IsConst|_|) = function
| Const (x, _) ->
    match x with
    | Num n -> 
        match n with
        | Real n -> TConst.TDecimal n
        | number.Integer n -> TConst.TInteger n
        |> OK
    | Str s when isNull s -> Error "string literal cannot be <null>"
    | Str s -> TStr s |> OK
    | Bool b -> TBool b |> OK
    | Const.Date d -> TDate d |> OK
    | Const.DateTime b -> TDateTime b |> OK
    |> Result.map TypedExpr.TConst
    |> Some
| _ -> None

let getFunctionCall op (lhs:TypedExpr) (rhs:TypedExpr) =
    let (|MethodInfo|_|) name (t:Type)=
        let type' = t.GetBCLType
        let mi = type'.GetMethod (name, [| type'; type' |])
        if isNull mi then None
        else Some mi
    match op, lhs.Type with
    | Plus, MethodInfo "op_Addition" mi 
    | Plus, MethodInfo "Concat" mi 
    | Minus, MethodInfo "op_Subtraction" mi 
    | Divide, MethodInfo "op_Division" mi 
    | Multiply, MethodInfo "op_Multiply" mi 
    | Concat, MethodInfo "op_Addition" mi 
    | Concat, MethodInfo "Concat" mi 
    | Equals, MethodInfo "op_Equality" mi 
    | Inequality, MethodInfo "op_Inequality" mi 
    | Greater, MethodInfo "op_GreaterThan" mi 
    | Less, MethodInfo "op_LessThan" mi 
    | LessOrEqual, MethodInfo "op_LessThanOrEqual" mi 
    | GreaterOrEqual, MethodInfo "op_GreaterThanOrEqual" mi 
        -> TFunctionCall (mi, Type.ToType mi.ReturnType, [lhs; rhs], true) |> OK
    | op, type' -> sprintf "Unsupported operator %A for type %A" op type' |> Error


let toTypedSyntaxTree (fs:Map<FunName, FunDef>) (refs:Map<RefName, RefDef>) expr : Result<TypedExpr, string> =
    let rec (|IsOperatorCall|_|) = function
        | OperatorCall (op, lhs, rhs, _) ->
            match toTypedSyntaxTree' lhs, toTypedSyntaxTree' rhs with
            | Error txt, _
            | _, Error txt
                -> Error txt
            | OK lhs, OK rhs ->
                match (lhs.Type, rhs), (rhs.Type, lhs) with
                | (Integer, rhs), (Integer, lhs)
                | (Boolean, rhs), (Boolean, lhs) ->
                    match op with
                    | Plus | Minus | Multiply | Concat when lhs.Type = Integer ->
                        TOperatorCall (op, lhs, rhs, lhs.Type) |> OK
                    | Divide when lhs.Type = Integer ->
                        let ctor = typeof<Rational>.GetConstructor([|typeof<int>; typeof<int>|])
                        TCallCtor(ctor, [ lhs; rhs]) |> OK
                    | Equals | Greater | Less | Inequality | LessOrEqual | GreaterOrEqual -> 
                        TOperatorCall (op, lhs, rhs, Boolean) |> OK
                    | _ -> sprintf "Operator %A is not supported for type %A" op lhs.Type |> Error
                | AreTypesCompatible rhs, _ -> getFunctionCall op lhs rhs
                | _, AreTypesCompatible lhs -> getFunctionCall op lhs rhs
                | NotCompatibleTypes (a,b), _ 
                | _, NotCompatibleTypes (a,b) 
                    -> sprintf "Types %A & %A are not compatible (in operator %A)" a b op |> Error
            |> Some
        | _ -> None
    and toTypedSyntaxTree' expr : Result<TypedExpr, string> =
        match expr with
        | IsOperatorCall e
        | IsConst e -> e
        | Reference (refName, _) when refName <> null -> 
            match refs.TryFind refName with
            | Some def -> TReference (refName, def.Type) |> OK
            | None -> 
                match fs.TryFind refName with
                | Some def when def.MethodInfo.GetParameters().Length = 0 ->
                    TFunctionCall (def.MethodInfo, def.ReturnType, [], def.IsReferntionalTransparent) |> OK
                | _ -> "unknown reference " + refName |> Error
        | Group (e, _) -> toTypedSyntaxTree' e
        | Negate (e, _) -> 
            let mi = typeof<decimal>.GetMethod ("op_UnaryNegation", [| typeof<decimal>; |])
            
            match toTypedSyntaxTree' e with
            | OK expr when expr.Type = Integer || expr.Type = Boolean
                -> TNegate expr |> OK
            | OK expr when expr.Type = Decimal && mi <> null
                -> TFunctionCall (mi, Decimal, [expr], true)  |> OK
            | Error _ as e -> e
            | OK expr -> sprintf "Negation of type %A is not supported" expr.Type |> Error
        | FunctionCall (name, ps, _) when name <> null ->
            match fs.TryFind name with
            | Some def when def.Parameters.Length <> ps.Length -> 
                sprintf "Function %s is expecting to have %i arguments, but supplied %i" 
                    name def.Parameters.Length  ps.Length
                |> Error
            | None -> "Unknown function " + name |> Error
            | Some def ->
                let psTypes = ps |> List.map toTypedSyntaxTree'
                let types, errors = psTypes |> List.partition (function OK _ -> true | _ -> false)
                if errors.Length > 0 then
                    errors |> List.head |> Result.unwrapError |> Error
                else
                    let types = types |> List.choose (function OK x->Some x | _ -> None)
                    let params', errors = 
                        types
                        |> List.zip def.Parameters
                        |> List.map (function AreTypesCompatible expr -> OK expr 
                                              | NotCompatibleTypes (a,b) -> sprintf "Types %A, %A are not compatible" a b |> Error)
                        |> List.partition (function OK _ -> true | _ -> false)

                    if errors.Length > 0 then
                        errors |> List.head |> Result.unwrapError |> Error
                    else
                        TFunctionCall (def.MethodInfo, def.ReturnType, params' |> List.map Result.unwrap, def.IsReferntionalTransparent) |> OK
        | Reference (_)
        | FunctionCall (_) ->
            sprintf "Unuported element %A " expr |>  Error 
    match expr with
    | OK expr -> toTypedSyntaxTree' expr  
    | Error (line:uint32, txt) -> sprintf "Error at line %i. Message = %s" line txt |> Error
