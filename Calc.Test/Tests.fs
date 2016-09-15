namespace Calc.Test
open NUnit.Framework

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
    let ``-1*(-1) is 1`` () = "-1*(-1)" == 1
    
    [<Test>] 
    let ``1=1 is true`` () = "1=1" == true
    
    [<Test>]
    let ``true=false is false`` () = "true=false" == false
    
    [<Test>] 
    let ``IF(1>2,5,7) is "7"`` () = "IF(1>2,5,7)" == "7"

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

