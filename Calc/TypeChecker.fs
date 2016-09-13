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
| Array of Type

type TypedExpr = 
| TConstStr of string
| TConstNum of number
| TConstBool of bool
| TFunctionCall of name:FunctionName * returnType:Type * TypedExpr list
| TNegate of TypedExpr
| TOperatorCall of operator * lhs:TypedExpr * rhs:TypedExpr 
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
                            | TConvertType (newType = t) 
                            | TReference (refType = t)
                            | TFunctionCall (returnType = t) -> t
                            | TNegate expr 
                            | TGroup expr
                            | TOperatorCall (lhs = expr)
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
    | Array a, Array e -> areCompatibleTypes a e
    | _ -> false

let (|AreTypesCompatible|NotCompatibleTypes|) (expected, expr:TypedExpr) = 
    let actual = expr.GetType
    if actual = expected then AreTypesCompatible expr 
    elif areCompatibleTypes actual expected then
         AreTypesCompatible(TConvertType (actual, expected, expr))
    else NotCompatibleTypes (actual, expected)

let rec getExprType(fs:Map<FunName, FunDef>) (refs:Map<RefName, RefDef>) =
    function
    | FunctionCall (name, _) ->
        match fs.TryFind name with
        | Some def -> OK def.ReturnType
        | None -> "unknown function " + name |> Error
    | Negate expr
    | Group expr -> getExprType fs refs expr
    | ConstBool _ -> OK Boolean
    | ConstNum t -> 
        match t with
        | number.Integer _ -> OK Integer
        | Real _ -> OK Decimal
    | ConstStr _ -> OK String
    | OperatorCall (op, lhs, _) ->
        match op with
        |Plus | Minus | Multiply | Divide (*| Power*) -> getExprType fs refs lhs
        |Equals | Greater | Less (*| GreaterOrEqual | LessOrEqual | NotEqual*) -> OK Boolean
    | Reference refName ->
        match refs.TryFind refName with
        | Some def -> OK def.Type
        | None -> "unknown reference " + refName |> Error
//
//let check (fs:Map<FunName, FunDef>) (refs:Map<RefName, RefDef>) expr =
//    let getExprType = getExprType fs refs
//    let rec check' (expr:Expr) : Result<Type,string> =
//        match expr with
//        | ConstNum _
//        | ConstStr _
//        | Reference _ 
//        | ConstBool _ -> getExprType expr
//        | Negate expr
//        | Group expr -> check' expr
//        | OperatorCall (op, lhs, rhs) ->
//            let opType = getExprType expr
//            let rhsType = check' rhs
//            let lhsType = check' lhs
//            match opType, lhsType, rhsType with
//            | Error txt, _ , _
//            | _, Error txt, _
//            | _, _, Error txt
//                -> Error txt
//            | OK opType, OK lhsType, OK rhsType ->
//                if areCompatibleTypes lhsType rhsType then OK opType
//                else sprintf "Types %A, %A, %A are compatible (in operator %A)" opType lhsType rhsType op |> Error
//                     
//        | FunctionCall (name, ps) ->
//            match fs.TryFind name with
//            | Some def when def.Parameters.Length <> ps.Length -> 
//                sprintf "Function %s is expecting to have %i arguments, but supplied %i" 
//                    name def.Parameters.Length  ps.Length
//                |> Error
//            | None -> "Unknown function " + name |> Error
//            | Some def ->
//                let psTypes = ps |> List.map check'
//                let types, errors = psTypes |> List.partition (function OK _ -> true | _ -> false)
//                if errors.Length > 0 then
//                    errors 
//                    |> List.head
//                    |> Result.unwrapError
//                    |> Error
//                else
//                    let types = types |> List.choose (function OK x->Some x | _ -> None)
//                    let errors = 
//                        def.Parameters 
//                        |> List.zip types 
//                        |> List.filter (fun (actual, expected) -> areCompatibleTypes actual expected |> not)
//                    if errors.Length > 0 then 
//                        let act, exp = errors.Head
//                        sprintf "Type %A is not compatible with %A (in function %s)" act exp name |> Error
//                    else
//                        OK def.ReturnType
//    match expr with
//    | OK expr -> 
//        match check' expr with
//        | OK t -> OK (expr, t)
//        | Error txt -> Error txt 
//    | Error (line:uint32, txt) -> sprintf "Error at line %i. Message = %s" line txt |> Error


let toTypedSyntaxTree (fs:Map<FunName, FunDef>) (refs:Map<RefName, RefDef>) expr =
    let getExprType = getExprType fs refs
    
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
            match getExprType expr, toTypedSyntaxTree' lhs, toTypedSyntaxTree' rhs with
            | Error txt, _, _
            | _, Error txt, _
            | _, _, Error txt
                -> Error txt
            | OK opType, OK lhs, OK rhs ->
                match (opType, lhs), (opType, rhs) with
                | AreTypesCompatible lhs, AreTypesCompatible rhs ->
                    TOperatorCall (op, lhs, rhs) |> OK
                | NotCompatibleTypes (a,b), _ 
                | _, NotCompatibleTypes (a,b) 
                    -> sprintf "Types %A, %A are compatible (in operator %A)" a b op |> Error
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
    | OK expr -> 
        match toTypedSyntaxTree' expr with
        | OK expr -> OK expr
        | Error txt -> Error txt 
    | Error (line:uint32, txt) -> sprintf "Error at line %i. Message = %s" line txt |> Error