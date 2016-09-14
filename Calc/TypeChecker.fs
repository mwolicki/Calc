module TypeChecker
open Analyse
open Tokenizer
open Core

type Type =
| String
| Decimal
| Integer
| Boolean
| Unit
with member t.GetBCLType =
        match t with
        | String -> typeof<string>
        | Decimal -> typeof<decimal>
        | Integer -> typeof<int>
        | Boolean -> typeof<bool>
        | Unit -> typeof<unit>


type TypedExpr = 
| TConstStr of string
| TConstNum of number
| TConstBool of bool
| TFunctionCall of name:FunctionName * returnType:Type * TypedExpr list
| TNegate of TypedExpr
| TOperatorCall of operator * lhs:TypedExpr * rhs:TypedExpr * opType : Type
| TReference of name:RefName * refType:Type
| TGroup of TypedExpr
| TConvertType of currentType: Type * newType: Type * expr:TypedExpr
with 
    member expr.GetType with get () = 
                            match expr with
                            | TConstBool _ -> Boolean
                            | TConstStr _ -> String
                            | TConstNum (number.Integer _) -> Integer
                            | TConstNum (number.Real _) -> Decimal
                            | TOperatorCall (opType = t)
                            | TConvertType (newType = t) 
                            | TReference (refType = t)
                            | TFunctionCall (returnType = t) -> t
                            | TNegate expr 
                            | TGroup expr
                                -> expr.GetType 
type FunName = string

type RefDef =
    { Name : RefName
      Type : Type }
type FunDef = 
    { Name : FunName
      MethodInfo : System.Reflection.MethodInfo }
with
    member def.ReturnType = def.MethodInfo.ReturnType |> FunDef.GetType
    member def.Parameters = 
        def.MethodInfo.GetParameters ()
        |> Array.map (fun p -> p.ParameterType |> FunDef.GetType)
        |> List.ofArray
    static member GetType t = 
        if typeof<System.String> = t then String
        elif typeof<System.Decimal> = t then Decimal
        elif typeof<System.Double> = t then Decimal
        elif typeof<System.Single> = t then Decimal
        elif typeof<System.Boolean> = t then Boolean
        elif typeof<System.UInt64> = t then Integer
        elif typeof<System.Int64> = t then Integer
        elif typeof<System.UInt32> = t then Integer
        elif typeof<System.Int32> = t then Integer
        elif typeof<System.UInt16> = t then Integer
        elif typeof<System.Int16> = t then Integer
        elif typeof<System.Byte> = t then Integer
        elif typeof<System.SByte> = t then Integer
        elif typeof<System.Void> = t then Unit
        else
            failwithf "Unsupported type %O %A" t.FullName t.IsGenericParameter

let rec areCompatibleTypes actual expected = 
    match actual, expected with
    | a, b when a=b -> true
    | Integer, Decimal
    | Decimal, String
    | Integer, String
    | Boolean, String
        -> true
    | _ -> false

let (|AreTypesCompatible|NotCompatibleTypes|) (expected, expr:TypedExpr) = 
    let actual = expr.GetType
    if actual = expected then AreTypesCompatible expr 
    elif areCompatibleTypes actual expected then
         AreTypesCompatible(TConvertType (actual, expected, expr))
    else NotCompatibleTypes (actual, expected)

let toTypedSyntaxTree (fs:Map<FunName, FunDef>) (refs:Map<RefName, RefDef>) expr =
    let rec toTypedSyntaxTree' expr =
        match expr with
        | ConstNum n -> TConstNum n |> OK
        | ConstStr s -> TConstStr s |> OK
        | ConstBool b -> TConstBool b |> OK
        | Reference refName -> 
            match refs.TryFind refName with
            | Some def -> TReference (refName, def.Type) |> OK
            | None -> "unknown reference " + refName |> Error
        | Group e -> toTypedSyntaxTree' e |> Result.map TGroup 
        | Negate e -> toTypedSyntaxTree' e |> Result.map TNegate
        | OperatorCall (op, lhs, rhs) ->
            let getOperator (lhs:TypedExpr) (rhs:TypedExpr) =
                match op with
                | Plus | Minus | Divide | Multiply->
                    TOperatorCall (op, lhs, rhs, lhs.GetType)
                | Equals | Greater | Less -> 
                    TOperatorCall (op, lhs, rhs, Boolean)

            match toTypedSyntaxTree' lhs, toTypedSyntaxTree' rhs with
            | Error txt, _
            | _, Error txt
                -> Error txt
            | OK lhs, OK rhs ->
                match (lhs.GetType, rhs), (rhs.GetType, lhs) with
                | AreTypesCompatible rhs, _ -> getOperator lhs rhs |> OK
                | _, AreTypesCompatible lhs -> getOperator lhs rhs |> OK
                | NotCompatibleTypes (a,b), _ 
                | _, NotCompatibleTypes (a,b) 
                    -> sprintf "Types %A & %A are not compatible (in operator %A)" a b op |> Error
        | FunctionCall (name, ps) ->
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
                                              | NotCompatibleTypes (a,b) -> sprintf "Types %A, %A are compatible" a b |> Error)
                        |> List.partition (function OK _ -> true | _ -> false)

                    if errors.Length > 0 then
                        errors |> List.head |> Result.unwrapError |> Error
                    else
                        TFunctionCall (name, def.ReturnType, params' |> List.map Result.unwrap) |> OK
    match expr with
    | OK expr -> toTypedSyntaxTree' expr  
    | Error (line:uint32, txt) -> sprintf "Error at line %i. Message = %s" line txt |> Error