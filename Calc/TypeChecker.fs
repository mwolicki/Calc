module TypeChecker
open Analyse
open Tokenizer
open Core
open System.Reflection

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
| TFunctionCall of methodInfo:MethodInfo * returnType:Type * TypedExpr list
| TNegate of TypedExpr
| TOperatorCall of operator * lhs:TypedExpr * rhs:TypedExpr * opType : Type
| TReference of name:RefName * refType:Type
| TGroup of TypedExpr
| TConvertType of currentType: Type * newType: Type * expr:TypedExpr
with 
    member expr.Type = 
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
            -> expr.Type 
type FunName = string


let getType t =
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
    static member GetType t = getType t
        

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
    let actual = expr.Type
    if actual = expected then AreTypesCompatible expr 
    elif areCompatibleTypes actual expected then
         AreTypesCompatible(TConvertType (actual, expected, expr))
    else NotCompatibleTypes (actual, expected)

let toTypedSyntaxTree (fs:Map<FunName, FunDef>) (refs:Map<RefName, RefDef>) expr : Result<TypedExpr, string> =
    let rec toTypedSyntaxTree' expr : Result<TypedExpr, string> =
        let (|IsILOperator|_|) = function
        | OperatorCall (op, lhs, rhs) ->
            match toTypedSyntaxTree' lhs, toTypedSyntaxTree' rhs with
            | OK lhs, OK rhs when 
                (lhs.Type = Integer && rhs.Type = Integer) ||
                (lhs.Type = Boolean && rhs.Type = Boolean) ->
                    Some(op, lhs, rhs)
            | _ -> None
        | _ -> None

        match expr with
        | ConstNum n -> TConstNum n |> OK
        | ConstStr s when isNull s -> Error "string literal cannot be <null>"
        | ConstStr s -> TConstStr s |> OK
        | ConstBool b -> TConstBool b |> OK
        | Reference refName -> 
            match refs.TryFind refName with
            | Some def -> TReference (refName, def.Type) |> OK
            | None -> "unknown reference " + refName |> Error
        | Group e -> toTypedSyntaxTree' e |> Result.map TGroup 
        | Negate e -> 
            let mi = typeof<decimal>.GetMethod ("op_UnaryNegation", [| typeof<decimal>; |])
            
            match toTypedSyntaxTree' e with
            | OK expr when expr.Type = Integer || expr.Type = Boolean
                -> TNegate expr |> OK
            | OK expr when expr.Type = Decimal && mi <> null
                -> TFunctionCall (mi, Decimal, [expr])  |> OK
            | Error _ as e -> e
            | OK expr -> sprintf "Negation of type %A is not supported" expr.Type |> Error
        | IsILOperator (op, lhs, rhs) ->
            match op with
            | Plus | Minus | Divide | Multiply | Concat ->
                TOperatorCall (op, lhs, rhs, lhs.Type) |> OK
            | Equals | Greater | Less | Inequality | LessOrEqual | GreaterOrEqual -> 
                TOperatorCall (op, lhs, rhs, Boolean) |> OK
        | OperatorCall (op, lhs, rhs) ->
            let getFunctionCall (lhs:TypedExpr) (rhs:TypedExpr) =
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
                    -> TFunctionCall (mi, getType mi.ReturnType, [lhs; rhs]) |> OK
                | op, type' -> sprintf "Unsupported operator %A for type %A" op type' |> Error

            match toTypedSyntaxTree' lhs, toTypedSyntaxTree' rhs with
            | Error txt, _
            | _, Error txt
                -> Error txt
            | OK lhs, OK rhs ->
                match (lhs.Type, rhs), (rhs.Type, lhs) with
                | AreTypesCompatible rhs, _ -> getFunctionCall lhs rhs
                | _, AreTypesCompatible lhs -> getFunctionCall lhs rhs
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
                        TFunctionCall (def.MethodInfo, def.ReturnType, params' |> List.map Result.unwrap) |> OK
    match expr with
    | OK expr -> toTypedSyntaxTree' expr  
    | Error (line:uint32, txt) -> sprintf "Error at line %i. Message = %s" line txt |> Error