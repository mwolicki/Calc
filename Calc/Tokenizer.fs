﻿module Tokenizer
open System
open System.Text.RegularExpressions
open Core
open Calc.Lib

type operator = Plus | Minus | Multiply | Divide (*| Power*) | Equals | Inequality | Greater | Less | GreaterOrEqual | LessOrEqual  | Concat

type brakcet = Open | Close
type number = Real of decimal | Integer of int


type Token =
    | Bracket of brakcet
    | Seperator of char
    | NumberLiteral of number
    | DateLiteral of Date
    | DateTimeLiteral of DateTime
    | Text of string
    | StringLiteral of string
    | BoolLiteral of bool
    | Operator of operator
    | WhiteSpace

type TokenAddr = (Token * uint32)

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

let (|IsBoolLiteral|_|) s =
    match s with
    | StartsWith "true" _ -> Some (true, 4)
    | StartsWith "false" _ -> Some (false, 5)
    | _ -> None
    |> Option.map (fun (b,i) -> BoolLiteral b, i)

    
let (|IsDateTimeLiteral|_|) s =
    match s with
    | IsRegex "^\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\]" (str, i) ->
        match DateTime.TryParse (str.Substring(1, i-2)) with
        | true,  dt -> (DateTimeLiteral dt, i) |>Some
        | _ -> None
    | _ -> None


let (|IsDateLiteral|_|) s =
    match s with
    | IsRegex "^\[[0-9]{4}-[0-9]{2}-[0-9]{2}\]" (str, i) ->
        match DateTime.TryParse (str.Substring(1, i-2)) with
        | true,  dt -> (DateLiteral (Date dt), i) |>Some
        | _ -> None
    | _ -> None

let (|IsStrLiteral|_|) s =
    match s with
    | IsRegex "^[A-Za-z_][A-Za-z_0-9]*" m ->
        Some m
    | IsRegex "^`[^`]+`" (str, i) ->
        Some (str.Substring (1, str.Length - 2), i)
    | _ -> None
    |> Option.map (fun (s, i) -> StringLiteral s, i)

let (|IsStr|_|) s =
    match s with
    | IsRegex "^\"[^\"]*\"" (str, i)
    | IsRegex "^'[^']*'" (str, i) ->
        Some (str.Substring (1, str.Length - 2) |> Text, i)
    | _ -> None


let (|IsNumber|_|) s =
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
    |> Option.map (fun (num, i) -> NumberLiteral num, i)


let (|IsOperator|_|) s =
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
    |> Option.map (fun (x , i)-> Operator x, i)

let (|IsBracket|_|) s =
    match char s with
    | '(' -> Some Open
    | ')' -> Some Close
    | _ -> None
    |> Option.map (fun x-> Bracket x, 1)

let (|IsWhiteSpace|_|) s = tryRegex "^[\s]+" s |> Option.map (fun x-> WhiteSpace, x)

let (|IsSeperator|_|) s = 
    match char s with 
    | ',' | ';' as x -> Some (Seperator x, 1) 
    | _ -> None

let toToken =
    function
    | IsBracket t
    | IsOperator t
    | IsSeperator t
    | IsBoolLiteral t
    | IsDateTimeLiteral t
    | IsDateLiteral t
    | IsNumber t
    | IsStr t
    | IsStrLiteral t
    | IsWhiteSpace t
        -> Some t
    | _ -> None

let removeWhitespaces = List.choose (function WhiteSpace, _ -> None | p -> Some p)

let tokenize s =
    let rec tokenize' (s:string) pos curr =
        match toToken s with 
        | Some (token, length) when length > 0 -> 
            let l : TokenAddr list = (token, pos) :: curr
            if l.Length > 1000 then
                Error (0u, "Script is too long")
            elif s.Length <> length then
                tokenize' (s.Substring length) (pos + uint32 length) l
            else l |> List.rev |> removeWhitespaces |> Result.OK
        | _ -> Error (pos, sprintf "Cannot tokenize %s" s)
    if System.String.IsNullOrEmpty s then
        Error (0u, "Cannot parse an empty string")
    else tokenize' s 0u []
