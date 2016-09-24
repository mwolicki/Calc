module Tokenizer
open System
open System.Text.RegularExpressions
open Core
open Calc.Lib

type operator = Plus | Minus | Multiply | Divide (*| Power*) | Equals | Inequality | Greater | Less | GreaterOrEqual | LessOrEqual  | Concat

type brakcet = Open | Close
type number = Real of decimal | Integer of int


type Token =
    | Bracket of brakcet * startPos:uint32 * endPos : uint32
    | Seperator of char * startPos:uint32 * endPos : uint32
    | NumberLiteral of number * startPos:uint32 * endPos : uint32
    | DateLiteral of Date * startPos:uint32 * endPos : uint32
    | DateTimeLiteral of DateTime * startPos:uint32 * endPos : uint32
    | Text of string * startPos:uint32 * endPos : uint32
    | StringLiteral of string * startPos:uint32 * endPos : uint32
    | BoolLiteral of bool * startPos:uint32 * endPos : uint32
    | Operator of operator * startPos:uint32 * endPos : uint32
    | WhiteSpace of startPos:uint32 * endPos : uint32
    with member token.Lenght =
            match token with
            | Bracket (_, s, e) 
            | Seperator (_, s, e)  
            | NumberLiteral (_, s, e)  
            | DateLiteral (_, s, e)  
            | DateTimeLiteral (_, s, e)  
            | Text (_, s, e)  
            | StringLiteral (_, s, e)  
            | BoolLiteral (_, s, e)  
            | Operator (_, s, e)  
            | WhiteSpace (s, e)  
                -> int (e - s) + 1

         member token.Pos =
            match token with
            | Bracket (_, s, e) 
            | Seperator (_, s, e)  
            | NumberLiteral (_, s, e)  
            | DateLiteral (_, s, e)  
            | DateTimeLiteral (_, s, e)  
            | Text (_, s, e)  
            | StringLiteral (_, s, e)  
            | BoolLiteral (_, s, e)  
            | Operator (_, s, e)  
            | WhiteSpace (s, e)  
                -> (s,e)

let tryRegex pattern s =
    Regex pattern
    |> fun x -> x.Match s
    |> fun m -> 
        if m.Success then Some m.Length
        else None

let (|IsRegex|_|) p s = tryRegex p s |> Option.map (fun i -> s.Substring(0, i), i)
let (|StartsWith|_|) p (s:string) = if s.StartsWith (p, StringComparison.InvariantCultureIgnoreCase) then Some p else None
let char (s:string) = s.[0]
let (|IsChar|_|) ch s = if char s = ch then Some ch else None

let (|IsBoolLiteral|_|) (s, pos) =
    match s with
    | StartsWith "true" _ -> Some (true, 4)
    | StartsWith "false" _ -> Some (false, 5)
    | _ -> None
    |> Option.map (fun (b,i) -> BoolLiteral (b, pos, pos + uint32 i - 1u))

    
let (|IsDateTimeLiteral|_|) (s, pos) =
    match s with
    | IsRegex "^\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\]" (str, i) ->
        match DateTime.TryParse (str.Substring(1, i-2)) with
        | true,  dt -> DateTimeLiteral (dt, pos, pos + uint32 i - 1u) |>Some
        | _ -> None
    | _ -> None

    
let (|IsDateLiteral|_|) (s, pos) =
    match s with
    | IsRegex "^\[[0-9]{4}-[0-9]{2}-[0-9]{2}\]" (str, i) ->
        match DateTime.TryParse (str.Substring(1, i-2)) with
        | true,  dt -> DateLiteral (Date dt, pos, pos + uint32 i - 1u) |>Some
        | _ -> None
    | _ -> None


let (|IsRomanLiteral|_|) (s, pos) =
    match s with
    | IsRegex "^\[[IVXLCDM]+\]" (str, i) ->
        match RomanNum.TryParse (str.Substring(1, i-2)) with
        | Some num -> NumberLiteral (Integer num, pos, pos + uint32 i - 1u) |>Some
        | _ -> None
    | _ -> None


let (|IsStrLiteral|_|) (s, pos) =
    match s with
    | IsRegex "^[A-Za-z_][A-Za-z_0-9]*" m ->
        Some m
    | IsRegex "^`[^`]+`" (str, i) ->
        (str.Substring (1, str.Length - 2), i) |> Some
    | _ -> None
    |> Option.map (fun (s, i) -> StringLiteral (s, pos, pos + uint32 i - 1u))

let (|IsStr|_|) (s, pos) =
    match s with
    | IsRegex "^\"[^\"]*\"" (str, i)
    | IsRegex "^'[^']*'" (str, i) ->
        (str.Substring (1, str.Length - 2), pos, pos + uint32 i - 1u) |> Text |> Some
    | _ -> None


let (|IsNumber|_|) (s, pos) =
    let isDecimal (str:string) (pos:int) =
        match Decimal.TryParse (str.Replace("_", "")) with
        | true, v -> Some (Real v, pos)
        | _ -> None
    match s with
    | IsRegex "^[0-9][0-9_]*\.[0-9_]*" (str, pos)
    | IsRegex "^\.[0-9_]+" (str, pos) ->
        isDecimal str pos
    | IsRegex "^[0-9][0-9_]*" (str, pos) ->
        match Int32.TryParse (str.Replace("_", "")) with
        | true, v -> Some (Integer v, pos)
        | _ -> isDecimal str pos
    | _ -> None
    |> Option.map (fun (num, i) -> NumberLiteral (num, pos, pos + uint32 i - 1u))


let (|IsOperator|_|) (s, pos) =
    match s with
    | IsChar '+' _ -> Some (Plus, 1)
    | IsChar '-' _ -> Some (Minus, 1)
    | IsChar '*' _ -> Some (Multiply, 1)
    | IsChar '/' _ -> Some (Divide, 1)
    //| IsChar '^' _ -> Some (Power, 1)
    //| StartsWith "<>" _ -> Some (NotEqual, 2)
    | IsChar '=' _ -> Some (Equals, 1)
    | StartsWith ">=" _ -> Some (GreaterOrEqual, 2)
    | StartsWith "<>" _ 
    | StartsWith "!=" _ 
        -> Some (Inequality, 2)
    | IsChar '>' _ -> Some (Greater, 1)
    | StartsWith "<=" _ -> Some (LessOrEqual, 2)
    | IsChar '<' _ -> Some (Less, 1)
    | IsChar '&' _ -> Some (Concat, 1)

    | _ -> None
    |> Option.map (fun (x , i)-> Operator (x, pos, pos + uint32 i - 1u))

let (|IsBracket|_|) (s, pos) =
    match char s with
    | '(' -> Open |> Some
    | ')' -> Close |> Some
    | _ -> None
    |> Option.map (fun x-> Bracket (x, pos, pos))

let (|IsWhiteSpace|_|) (s, pos) = tryRegex "^[\s]+" s |> Option.map (fun x-> WhiteSpace (pos, pos + uint32 x - 1u))

let (|IsSeperator|_|) (s, pos) = 
    match char s with 
    | ',' | ';' as x -> Seperator (x, pos, pos) |> Some
    | _ -> None

let toToken (text, pos) : Token option =
    match text, pos with
    | IsBracket t
    | IsOperator t
    | IsSeperator t
    | IsBoolLiteral t
    | IsDateTimeLiteral t
    | IsDateLiteral t
    | IsRomanLiteral t
    | IsNumber t
    | IsStr t
    | IsStrLiteral t
    | IsWhiteSpace t
        -> Some t
    | _ -> None

let removeWhitespaces = List.choose (function WhiteSpace _ -> None | p -> Some p)

let tokenize s =
    let rec tokenize' (s:string) pos curr =
        match toToken (s, pos) with 
        | Some token when token.Lenght > 0 -> 
            let l = token :: curr
            if l.Length > 1000 then
                Error (0u, "Script is too long")
            elif s.Length <> token.Lenght then
                tokenize' (s.Substring token.Lenght) (pos + uint32 token.Lenght) l
            else l |> List.rev |> removeWhitespaces |> Result.OK
        | _ -> Error (pos, sprintf "Cannot tokenize %s" s)
    if System.String.IsNullOrEmpty s then
        Error (0u, "Cannot parse an empty string")
    else tokenize' s 0u []
