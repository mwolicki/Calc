namespace Calc.Test
open NUnit.Framework
open FsCheck
module Tests =

    let compileAndRun<'a> s = (Program.Compile.compile<'a> s).Invoke(Unchecked.defaultof<Calc.Lib.IReferenceAccessor>)

    let (==) a (b:'a) = Assert.AreEqual(b, compileAndRun<'a> a)

    [<Test>]
    let ``1 is 1`` () = "1" == 1
    
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

    type MathOps =
    | Add
    | Sub
    | Div
    | Mul
    with member op.Char =
            match op with
            | Add -> '+'
            | Sub -> '-'
            | Div -> '/'
            | Mul -> '*'
         member inline op.Calc (a, b)=
            match op with
            | Add -> a+b
            | Sub -> a-b
            | Div -> a/b
            | Mul -> a*b

    let inline catch f =
        try
            f () |> Choice1Of2
        with ex -> ex.GetType().FullName |> Choice2Of2

    [<Test>] 
    let ``check int operations`` () =
        let test (a:int) (op:MathOps) (b:int) =
            let actual = catch (fun () -> sprintf "%i %O %i" a op.Char b |> compileAndRun)
            let expected = catch (fun () -> op.Calc (a, b))
            actual = expected
        Check.QuickThrowOnFailure test

    [<Test>] 
    let ``check int/decima operations`` () =
        let test (a:int) (op:MathOps) (b:decimal) =
            let actual1 = catch (fun () ->sprintf "%i %O %M" a op.Char b |> compileAndRun)
            let expected1 = catch (fun () ->op.Calc (decimal a, b))
            let actual2 = catch (fun () ->sprintf "%M %O %i" b op.Char a |> compileAndRun)
            let expected2 = catch (fun () ->op.Calc (b, decimal a))
            actual1 = expected1 && actual2 = expected2 
        Check.QuickThrowOnFailure test

