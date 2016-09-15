﻿module Analyse
open Tokenizer
open Core

type FunctionName = string
type RefName = string

type Expr = 
| ConstStr of string
| ConstNum of number
| ConstBool of bool
| FunctionCall of name:FunctionName * Expr list
| Negate of Expr
| OperatorCall of operator * left:Expr * right:Expr 
| Reference of name:RefName
| Group of Expr

and ExprAddr = Expr * TokenAddr

let (|IsReference|_|) = function
    | StringLiteral s :: xs -> Some (Reference s, xs)
    | _ -> None

let operatorPrecedence = 
    [ Plus, 10
      Minus, 15
      Multiply, 20
      Divide, 20
      //Power, 30
      Equals, 0
      Greater, 0
      Less, 0
      //GreaterOrEqual, 0
      //LessOrEqual, 0
      //NotEqual, 0 
    ] |> Map.ofList

let rec applyOperatorPrecedence expr = 
    match expr with
    | ConstBool _
    | ConstNum _
    | ConstStr _
    | Reference _
    | Negate _
        -> expr
    | FunctionCall (name, ps) -> FunctionCall (name, ps |> List.map applyOperatorPrecedence)
    | Group expr -> applyOperatorPrecedence expr |> Group
    | OperatorCall (op, lhs, rhs) ->
        let lhs = applyOperatorPrecedence lhs
        let rhs = applyOperatorPrecedence rhs
        match lhs, rhs with
        | OperatorCall (op', lhs', rhs'), _ ->
            if operatorPrecedence.[op'] < operatorPrecedence.[op] then
                OperatorCall (op', OperatorCall (op, lhs, lhs'), rhs')
            else
                OperatorCall (op, OperatorCall(op', lhs', rhs'), rhs)
        | _, OperatorCall (op', lhs', rhs') ->
            if operatorPrecedence.[op'] < operatorPrecedence.[op] then
                OperatorCall (op', OperatorCall (op, lhs, lhs'), rhs')
            else
                OperatorCall (op, lhs, OperatorCall(op', lhs', rhs'))

        | _ -> expr
let rec (|IsFunctionCall|_|) (t: Token list) =
    let (|GetParams|_|) (t:Token list)  =
        let rec getParams (t:Token list) curr =
            match t with
            | Bracket Close :: ts -> Some([], ts)
            | Analyse (expr, (Seperator _ :: ts)) -> getParams ts (expr :: curr)
            | Analyse (expr, (Bracket Close :: ts)) -> Some(expr :: curr, ts)
            | _ -> None
        getParams t [] |> Option.map (fun (l, ts) -> List.rev l, ts)

    match t with
    | StringLiteral s :: Bracket Open :: ts ->
        match ts with
        |GetParams (ps, ts) ->
            Some (FunctionCall (s, ps), ts)
        | _ -> None
    | _ -> None
and (|IsNegate|_|) expr = 
    let (|Operation|_|) = function
       | IsLiteral x
       | IsGroup x
       | IsFunctionCall x
       | IsReference x -> Some x
       | _ -> None

    match expr with
    | Operator operator.Minus :: Operation (expr, ts) -> 
       (Negate expr, ts) |> Some
    | _ -> None
and (|IsGroup|_|) = function
     | Bracket Open ::  Analyse (expr, (Bracket Close :: ts)) -> 
        (Group expr, ts) |> Some
     | _ -> None
and (|IsLiteral|_|) = function
    | NumberLiteral n :: ts -> Some (ConstNum n, ts) 
    | BoolLiteral b :: ts -> Some (ConstBool b, ts)
    | Text s :: ts -> Some (ConstStr s, ts)
    | _ -> None
and (|Analyse|) (t:Token list) : Expr * Token list = analyse' [] t
and analyse' (res:Expr list) (t: Token list) : (Expr * Token list) =
    let getFirstOperand = 
        function
        | [r] -> r
        | rs -> failwithf "Unsupported combination of operations %A" rs
    match t with
    | IsLiteral (expr, ts)
    | IsGroup (expr, ts)
    | IsFunctionCall (expr, ts)
    | IsReference (expr, ts) -> analyse' (expr :: res) ts
    | Operator s :: ts when res.Length = 1 -> 
        let left = getFirstOperand res
        let right, ts = analyse' [] ts
        OperatorCall (s, left, right), ts
    | IsNegate (expr, ts) -> analyse' (expr :: res) ts
    | _ -> getFirstOperand res, t

let analyse (tokens) = 
    match tokens with 
    | OK t -> 
        match analyse' [] (t |> List.map fst) with
        | res, [] -> res |> applyOperatorPrecedence |> OK
        | _, t::_ -> Error (0u, sprintf "failed analysing %A" t)
    | Error (s, p) -> Error (s, p) 