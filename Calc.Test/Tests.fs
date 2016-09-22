namespace Calc.Test

module Tests =
    open Calc.Lib
    open TypeChecker
    open Compile
    open NUnit.Framework
    open FsCheck

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

    let accessor = 
        { new IReferenceAccessor with
          member __.GetInt name = 
            match name with
            | "i1" -> 1
            | "i2" -> 2
            | n -> n.GetHashCode()
          member __.GetBoolean _ = false
          member __.GetString _ = "text"
          member __.GetDecimal _ = 2m
          member __.GetDate _ = Calc.Lib.Date(1,2,3)
          member __.GetDateTime _ = System.DateTime.Now }

    let compileAndRun<'a> s = 
        Compile.compile'<'a> defaultFuncs refs s
        |> fun d -> d.Invoke accessor

    let (==) a (b:'a) = Assert.AreEqual(b, compileAndRun<'a> a)
    
    [<Test>]
    let ``1 is 1`` () = "1" == 1

    [<Test>]
    let ``[2016-09-11] is Date(2016-09-11)`` () = "[2016-09-11]" == Calc.Lib.Date(2016,09,11)


    [<Test>]
    let ``'1' & '2' is "12"`` () = "'1' & '2'" == "12"

    [<Test>]
    let ``1>=1 is true`` () = "1>=1" == true

    [<Test>]
    let ``1<=1 is true`` () = "1<=1" == true


    [<Test>]
    let ```d long name` + 1 is 3m`` () = "`d long name` + 1" == 3m

    [<Test>]
    let ``1_000_000 is 1000000`` () = "1_000_000" == 1000000

    [<Test>]
    let ``1_000_000.9_9_9 is 1000000.999`` () = "1_000_000.9_9_9" == 1000000.999m
    
    [<Test>]
    let ``1.0 is 1m`` () = "1.0" == 1m
    
    [<Test>]
    let ``-true is false`` () = "-true" == false

    [<Test>]
    let ``-false is true`` () = "-false" == true

    [<Test>]
    let ``-('a'<>'b') is false`` () = "-('a'<>'b')" == false
    
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
    let ``1.0>=1.0 is true`` () = "1.0>=1.0" == true
    
    [<Test>] 
    let ``1<=-2 is false`` () = "1<=-2" == false
    
    [<Test>] 
    let ``1<>1 is false`` () = "1<>1" == false

    [<Test>] 
    let ``'a'<>"a" is false`` () = "'a'<>\"a\"" == false
    
    [<Test>] 
    let ``123.456!=123.4567 is true`` () = "123.456!=123.4567" == true

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
        let expr = Optimizer.optimizer expr
        try
            (generateDynamicType<'a> fs expr :?> System.Func<IReferenceAccessor, 'a>)
            |> fun x -> x.Invoke accessor
            |> box |> Choice1Of2
        with 
            | e -> e.Message |> Choice2Of2

    open Analyse




    type DateGenerators =
        static member Version() =
            Gen.map (Calc.Lib.Date) Arb.generate<System.DateTime> |> Arb.fromGen

    type Generators =
        
        static member Version()=
            let dummyToken = [Tokenizer.WhiteSpace(0u,0u)]
            let funcs = defaultFuncs |> Seq.filter(fun x->x.Key <> "NOW")
            let availableReferences = Gen.oneof [ Gen.map (fun x-> Reference(x, dummyToken)) (refs |> Seq.map (fun kvp -> gen { return kvp.Key }) |> Gen.oneof) ]
            let availableFuncs = funcs |> Seq.map(fun kvp ->  kvp.Key, kvp.Value.Parameters.Length) |> Map.ofSeq
            let aFuncs = funcs |> Seq.map (fun kvp -> gen { return kvp.Key }) |> Gen.oneof

            let generator () =
                let dummyToken = [Tokenizer.WhiteSpace(0u,0u)]

                let genSingle () =
                    Gen.oneof [ Gen.map (fun x-> Const(x, dummyToken)) Arb.generate<Const>
                                availableReferences ]

                let rec generator = function
                | 0 -> genSingle()
                | n ->
                    let generator () = generator (n/3)
                    Gen.oneof [ Gen.map3 (fun a b c -> OperatorCall(a,b,c, dummyToken)) Arb.generate<Tokenizer.operator> (generator()) (generator())
                                Gen.map4 (fun f a b c ->
                                            match availableFuncs.[f] with
                                            | 0 -> FunctionCall(f, [], dummyToken)
                                            | 1 -> FunctionCall(f, [a], dummyToken)
                                            | 2 -> FunctionCall(f, [a; b], dummyToken)
                                            | _ -> FunctionCall(f, [a; b; c], dummyToken)) aFuncs (generator()) (generator()) (generator())
                                genSingle()
                                Gen.map (fun x-> Negate(x , dummyToken)) (generator())
                                Gen.map (fun x-> Group(x, dummyToken)) (generator()) ]
                Gen.sized generator
            Arb.fromGen (generator())

    [<Test>] 
    let ``random AST dont crash analyser`` () =
        let callMethod fs (expr:TypedExpr)  =
            match expr.Type with
            | Integer -> callMethod<int> fs expr
            | String -> callMethod<string>  fs expr
            | Decimal -> callMethod<decimal>  fs expr
            | Boolean -> callMethod<bool>  fs expr
            | Type.Date -> callMethod<Date>  fs expr
            | Type.DateTime -> callMethod<System.DateTime>  fs expr
            | UserDefined _ -> failwith "Userdefined types are not supported in tests"
        let test (tokens : Analyse.Expr) =
            match tokens |> Core.OK |> TypeChecker.toTypedSyntaxTree defaultFuncs refs with
            | Core.OK x -> callMethod defaultFuncs x |> ignore
            | Core.Error e -> ()

        Arb.register<DateGenerators>() |> ignore
        Arb.register<Generators>() |> ignore
        Check.One({ Config.QuickThrowOnFailure with MaxTest = 100 },test)


    
    let compareWithOracle<'a> fs expr = 
            let actual = callMethod<'a> fs expr
            let expected = Eval.eval expr accessor
            Assert.AreEqual(expected, actual)

    [<Test>] 
    let ``compere results with oracle`` () =

        let test (expr:TypedExpr) fs =
            match expr.Type with
            | Type.Integer ->compareWithOracle<int>
            | String -> compareWithOracle<string>
            | Decimal -> compareWithOracle<decimal>
            | Boolean -> compareWithOracle<bool>
            | Type.Date -> compareWithOracle<Date>
            | Type.DateTime -> compareWithOracle<System.DateTime>
            | UserDefined _ -> failwith "Userdefined types are not supported in tests"



            |> fun f -> f fs expr

        let test (tokens : Analyse.Expr) =
            match tokens |> Core.OK |> TypeChecker.toTypedSyntaxTree defaultFuncs refs with
            | Core.OK x -> test x defaultFuncs
            | Core.Error e -> printfn "%A" e

        
        Arb.register<DateGenerators>() |> ignore
        Arb.register<Generators>() |> ignore
        Check.One( { Config.QuickThrowOnFailure with MaxTest = 400 },test)




//    let x = getTypedExpr defaultFuncs refs "TEXT(1+3*6+SIN(100)-21+'this is my text' & 'hello!')" |> Core.Result.unwrap
//
//    let c = compile'<string> defaultFuncs refs "TEXT(1+3*6+SIN(100)-21+'this is my text' & 'hello!')"
//
//
//    for i=0 to 100000000 do
//        c.Invoke(accessor) |> ignore
//
//    for i=0 to 100000000 do
//        Eval.eval x accessor |> ignore

