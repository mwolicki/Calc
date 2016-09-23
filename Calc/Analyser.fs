module Analyse
open Tokenizer
open Core
open Calc.Lib

type FunctionName = string
type RefName = string

type Const =
    | Str of string
    | Num of number
    | Bool of bool
    | Date of Date
    | DateTime of System.DateTime

type Expr = 
| Const of Const * Token list
| FunctionCall of name:FunctionName * Expr list * Token list
| Negate of Expr * Token list
| OperatorCall of operator * left:Expr * right:Expr * Token list
| Reference of name:RefName * Token list
| Group of Expr * Token list
with
    member expr.ToDisplay() = 
        match expr with
        | Const (c, _) -> 
            match c with
             | Str x -> x.ToString()
             | Num x -> 
                match x with
                | number.Integer i -> i.ToString()
                | number.Real r -> r.ToString()
             | Bool x -> x.ToString()
             | Date x -> x.ToString() |> sprintf "[%s]"
             | DateTime x -> x.ToString() |> sprintf "[%s]"
        | FunctionCall (name, es,_) ->
            sprintf "%s(%s)" name (es |> Seq.map (fun x->x.ToDisplay()) |> String.concat ", ")
        | Negate (x,_) -> x.ToDisplay() |> sprintf "-%s"
        | OperatorCall (op, lhs, rhs,_) -> 
            let op = 
                match op with
                 | Plus -> "+"
                 | Minus -> "-"
                 | Multiply -> "*"
                 | Divide -> "/"
                 | Equals -> "="
                 | Inequality  -> "<>"
                 | Greater  -> ">"
                 | Less  -> "<"
                 | GreaterOrEqual -> ">=" 
                 | LessOrEqual   -> "<="
                 | Concat -> "&"
            sprintf "(%s %s %s)" (lhs.ToDisplay()) (op.ToString()) (rhs.ToDisplay())
        | Reference (name, _) -> sprintf "`%s`" name
        | Group (x, _) -> x.ToDisplay() |> sprintf "(%s)"

let (|IsReference|_|) = function
    | (StringLiteral (s, _, _) as t) :: xs -> Some (Reference (s, [t]) , xs)
    | _ -> None

let operatorPrecedence = 
    [ Plus, 10
      Concat, 10
      Minus, 15
      Multiply, 20
      Divide, 30
      //Power, 30
      Equals, 0
      Greater, 0
      Less, 0
      GreaterOrEqual, 0
      LessOrEqual, 0
      Inequality, 0 
    ] |> Map.ofList

let rec applyOperatorPrecedence expr = 
    match expr with
    | Const _
    | Reference _ -> expr
    | Negate (expr, ts) -> Negate(applyOperatorPrecedence expr, ts)
    | FunctionCall (name, ps, tokens) -> FunctionCall (name, ps |> List.map applyOperatorPrecedence, tokens)
    | Group (expr, tokens) -> Group (applyOperatorPrecedence expr, tokens)
    | OperatorCall (op, lhs, rhs, tokens) ->
        let lhs = applyOperatorPrecedence lhs
        let rhs = applyOperatorPrecedence rhs
        match lhs, rhs with
        | OperatorCall (op_, lhs_, rhs_, tokens'), _ when operatorPrecedence.[op_] < operatorPrecedence.[op] ->
                OperatorCall (op_, OperatorCall (op, lhs, lhs_, tokens'), rhs_, tokens)
        | _, OperatorCall (op_, lhs_, rhs_, tokens') when operatorPrecedence.[op_] < operatorPrecedence.[op] ->
                OperatorCall (op_, OperatorCall (op, lhs, lhs_, tokens), rhs_, tokens')
        | _ -> OperatorCall (op, lhs, rhs, tokens)
let rec (|IsFunctionCall|_|) (t: Token list) =
    let (|GetParams|_|) (t:Token list)  =
        let rec getParams (t:Token list) curr =
            match t with
            | (Bracket (Close,_, _) as closeBracket) :: ts -> Some([], ts, closeBracket)
            | Analyse (expr, (Seperator _ :: ts)) -> getParams ts (expr :: curr)
            | Analyse (expr, ((Bracket (Close, _,_) as closeBracket) :: ts)) -> Some(expr :: curr, ts, closeBracket)
            | _ -> None
        getParams t [] |> Option.map (fun (l, ts, closeBracket) -> List.rev l, ts, closeBracket)

    match t with
    | (StringLiteral (s, _, _) as nameToken) :: (Bracket (Open, _, _) as openToken) :: ts ->
        match ts with
        |GetParams (ps, ts, closeToken) ->
            Some (FunctionCall (s, ps, [nameToken; openToken; closeToken]), ts)
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
    | (Operator (operator.Minus,_,_) as token) :: Operation (expr, ts) -> 
       (Negate (expr, [token]), ts) |> Some
    | _ -> None
and (|IsGroup|_|) = function
     | (Bracket (Open, _,_) as openToken) ::  Analyse (expr, ((Bracket (Close, _, _) as closeToken) :: ts)) -> 
        (Group (expr, [openToken; closeToken]), ts) |> Some
     | _ -> None
and (|IsLiteral|_|) = function
    | (NumberLiteral (n, _,_) as t) :: ts -> Some (Const (Num n, [t]), ts) 
    | (BoolLiteral (b, _,_) as t) :: ts -> Some (Const (Bool b, [t]), ts)
    | (Text (s, _, _) as t) :: ts -> Some (Const (Str s, [t]), ts)
    | (DateLiteral (s, _, _) as t) :: ts -> Some (Const(Date s , [t]), ts)
    | (DateTimeLiteral (s, _, _) as t) :: ts -> Some (Const (DateTime s, [t]), ts)
    | _ -> None
and (|Analyse|_|) (t:Token list)= 
    match analyse' [] t with
    | OK x -> Some x
    | _ -> None
and analyse' (res:Expr list) (t: Token list) =
    let getFirstOperand = 
        function
        | [r] -> OK r
        | rs -> sprintf "Unsupported combination of operations %A" rs |> Error 
    match t with
    | IsLiteral (expr, ts)
    | IsGroup (expr, ts)
    | IsFunctionCall (expr, ts)
    | IsReference (expr, ts) -> analyse' (expr :: res) ts
    | (Operator (s, _, _) as token) :: ts when res.Length = 1 -> 
        let left = getFirstOperand res
        let right = analyse' [] ts
        match left, right with
        | OK left, (OK (right, ts)) ->
            (OperatorCall (s, left, right, [token]), ts) |> OK
        | Error x, _
        | _, Error x ->
            Error x
    | IsNegate (expr, ts) -> analyse' (expr :: res) ts
    | _ -> getFirstOperand res |> Result.map (fun r-> r, t)

let analyse (tokens) = 
    match tokens with 
    | OK t -> 
        match analyse' [] t with
        | OK (res, []) -> res |> applyOperatorPrecedence |> OK
        | OK(_, t::_) -> Error (0u, sprintf "failed analysing %A" t)
        | Error (s) -> Error (0u, s) 
    | Error (s, p) -> Error (s, p) 