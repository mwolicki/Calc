﻿namespace Calc.Test
open NUnit.Framework
open FsCheck
module Tests =
    open Calc.Lib
    open TypeChecker
    open Compile

    let refs =
        [ { Name = "i1"; Type = Integer }
          { Name = "i2"; Type = Integer }
          { Name = "s1"; Type = String }
          { Name = "s2"; Type = String }
          { Name = "b1"; Type = Boolean }
          { Name = "b2"; Type = Boolean }
          { Name = "d1"; Type = Decimal }
          { Name = "d2"; Type = Decimal }
          { Name = "d long name"; Type = Decimal } ]
        |> List.map (fun x-> x.Name, x)
        |> Map.ofList

    let rnd = new System.Random()

    let accessor = 
        { new IReferenceAccessor with
          member __.GetInt name = 
            match name with
            | "i1" -> 1
            | "i2" -> rnd.Next ()
            | _ -> -5
          member __.GetBoolean _ = false
          member __.GetString _ = "text"
          member __.GetDecimal _ = 2m }
          

    let compileAndRun<'a> s = 
        Compile.compile'<'a> defaultFuncs refs s
        |> fun d -> d.Invoke accessor

    let (==) a (b:'a) = Assert.AreEqual(b, compileAndRun<'a> a)

    [<Test>]
    let ``1 is 1`` () = "1" == 1
    
    [<Test>]
    let ```d long name` + 1 is 3m`` () = "`d long name` + 1" == 3m

    [<Test>]
    let ``1_000_000 is 1000000`` () = "1_000_000" == 1000000

    [<Test>]
    let ``1_000_000.9_9_9 is 1000000.999`` () = "1_000_000.9_9_9" == 1000000.999m
    
    [<Test>]
    let ``1.0 is 1m`` () = "1.0" == 1m
    
    [<Test>] 
    let ``'text abc' is "text abc"`` () = "'text abc'" == "text abc"
    
    [<Test>] 
    let ``1 + 2 + 3 is 6`` () = "1 + 2 + 3" == 6

    [<Test>]
    let ``1 + 2.0 + 3 is 6m`` () = "1 + 2.0 + 3" == 6m
    
    [<Test>] 
    let ``-1 is -1`` () = "-1" == -1
    
    [<Test>] 
    let ``-1.0 is -1m`` () = "-1.0" == -1m

    [<Test>] 
    let ``-1*(-1) is 1`` () = "-1*(-1)" == 1
    
    [<Test>] 
    let ``1=1 is true`` () = "1=1" == true
    
    [<Test>]
    let ``true=false is false`` () = "true=false" == false
    
    [<Test>] 
    let ``IF(1>2,5,7) is "7"`` () = "IF(1>2,5,7.0)" == "7.0"

    [<Test>] 
    let ``IF(1>2,-5,7) is "7"`` () = "IF(1>2,-5,7)" == "7"
    
    [<Test>] 
    let ``4-4*0+1 is 5`` () = "4-4*0+1" == 5

    [<Test>] 
    let ``(4-4)*0+1 is 1`` () = "(4-4)*0+1" == 1

    [<Test>] 
    let ``(4.0-4.0)*0.0+1.0/1.0 is 1m`` () = "(4.0-4.0)*0.0+1.0/1.0" == 1m

    [<Test>] 
    let ``'TEXT' & "text" is "TEXTtext"`` () = "'TEXT' & \"text\"" == "TEXTtext"
    
    [<Test>] 
    let ``1>=1 is true`` () = "1>=1" == true

    [<Test>] 
    let ``1.0>=1.0 is true`` () = "1.0>=1.0" == true
    
    [<Test>] 
    let ``1<=-2 is false`` () = "1<=-2" == false
    
    [<Test>] 
    let ``1<>1 is false`` () = "1<>1" == false

    [<Test>] 
    let ``'a'<>"a" is false`` () = "'a'<>\"a\"" == false
    
    [<Test>] 
    let ``123.456!=123.4567 is true`` () = "123.456!=123.4567" == true

    
    [<Test>] 
    let ``SIN`` () = 
        for i=0 to 1000 do
            sprintf "TEXT(SIN(%i))" i |> compileAndRun<string> |> printfn "%A"


    type MathOps = Add | Sub | Div | Mul
    with member op.Str =
            match op with
            | Add -> "+" | Sub -> "-"
            | Div -> "/" | Mul -> "*"
         member inline op.Calc (a, b)=
            match op with
            | Add -> a+b | Sub -> a-b
            | Div -> a/b | Mul -> a*b

    type BoolOps = Equals | Inequals | Greater | Less | GreaterOrEqual | LessOrEqual
    with member op.Str =
            match op with
            | Equals -> "=" | Inequals -> "<>"
            | GreaterOrEqual -> ">=" | Greater -> ">"
            | LessOrEqual -> "<=" | Less -> "<"
         member inline op.Calc (a, b)=
            match op with
            | Equals -> a=b | Inequals -> a<>b
            | GreaterOrEqual -> a>=b | Greater -> a>b
            | LessOrEqual -> a<=b | Less -> a<b
    let inline catch f =
        try
            f () |> Choice1Of2
        with ex -> ex.GetType().FullName |> Choice2Of2

    [<Test>] 
    let ``check int math operations`` () =
        let test (a:int) (op:MathOps) (b:int) =
            let actual = catch (fun () -> sprintf "%i %O %i" a op.Str b |> compileAndRun<int>)
            let expected = catch (fun () -> op.Calc (a, b))
            actual = expected
        Check.QuickThrowOnFailure test

    [<Test>] 
    let ``check int/decima math operations`` () =
        let test (a:int) (op:MathOps) (b:decimal) =
            let actual1 = catch (fun () ->sprintf "%O %O %O" a op.Str b |> compileAndRun<decimal>)
            let expected1 = catch (fun () ->op.Calc (decimal a, b))
            let actual2 = catch (fun () ->sprintf "%O %O %O" b op.Str a |> compileAndRun<decimal>)
            let expected2 = catch (fun () ->op.Calc (b, decimal a))
            actual1 = expected1 && actual2 = expected2 
        Check.QuickThrowOnFailure test

    [<Test>] 
    let ``check int bool operations`` () =
        let test (a:int) (op:BoolOps) (b:int) =
            let actual = catch (fun () -> sprintf "%i %O %i" a op.Str b |> compileAndRun<bool>)
            let expected = catch (fun () -> op.Calc (a, b))
            Assert.AreEqual(expected, actual)
        Check.QuickThrowOnFailure test
        
    [<Test>] 
    let ``check int/decima bool operations`` () =
        let test (a:int) (op:BoolOps) (b:decimal) =
            let actual1 = catch (fun () ->sprintf "%O %O %O" a op.Str b |> compileAndRun<bool>)
            let expected1 = catch (fun () ->op.Calc (decimal a, b))
            let actual2 = catch (fun () ->sprintf "%O %O %O" b op.Str a |> compileAndRun<bool>)
            let expected2 = catch (fun () ->op.Calc (b, decimal a))
            Assert.AreEqual(expected1, actual1)
            Assert.AreEqual(expected2, actual2)
        Check.QuickThrowOnFailure test

    [<Test>] 
    let ``random tokens don't crash analyser`` () =
        let test (tokens : Tokenizer.Token list) =
            tokens |> Analyse.analyse' [] |> ignore
        Check.QuickThrowOnFailure test

    
    [<Test>] 
    let ``random string doesn't crash tokenizer`` () =
        let test str =
            Tokenizer.tokenize str |> ignore
        Check.QuickThrowOnFailure test

    open TypeChecker
    open Emitter

    let callMethod<'a> fs expr = 
        (generateDynamicType<'a> fs expr :?> System.Func<IReferenceAccessor, 'a>)
        |> fun x -> x.Invoke accessor
        |> ignore

    open Analyse
    [<Test>] 
    let ``random AST dont crash analyser`` () =
        let callMethod fs (expr:TypedExpr)  =
            match expr.Type with
            | Integer -> callMethod<int> fs expr
            | String -> callMethod<string>  fs expr
            | Decimal -> callMethod<decimal>  fs expr
            | Boolean -> callMethod<bool>  fs expr
            | Unit -> callMethod<unit>  fs expr

        let rec getRefs (expr: Analyse.Expr) =
            match expr with
            | ConstStr _
            | ConstNum _
            | ConstBool _ -> Set.empty, Set.empty
            | FunctionCall (name, es) ->
                let init = Set.empty, Set [name]
                es
                |> List.map getRefs 
                |> List.fold (fun (rs, fs) (rs', fs') -> Set.union rs rs', Set.union fs fs') init
            | OperatorCall (_, a, b) ->
                 let rs, fs = getRefs a
                 let rs', fs' = getRefs b
                 Set.union rs rs', Set.union fs fs'
            | Reference name -> Set [name], Set.empty
            | Negate e
            | Group e -> getRefs e

        let test (tokens : Analyse.Expr) =
            let rs, fs = getRefs tokens
            let mi = defaultFuncs.["TEXT"].MethodInfo
            let fs = fs |> Seq.map (fun x->x, {Name = x; MethodInfo = mi}) |> Map.ofSeq
            let rs = rs |> Seq.map (fun x->x, {Name = x; Type = Integer}) |> Map.ofSeq

            match tokens |> Core.OK |> TypeChecker.toTypedSyntaxTree fs rs with
            | Core.OK x -> callMethod fs x
            | Core.Error e -> printfn "%A" e
        Check.QuickThrowOnFailure test
