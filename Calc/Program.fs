#if INTERACTIVE
#r @"..\Calc.Lib\bin\Release\Calc.Lib.dll"
#load "Result.fs"
#load "Tokenizer.fs"
#load "Analyser.fs"
#load "TypeChecker.fs"
#load "Emitter.fs"
#endif


open TypeChecker

open System.Reflection
open Calc.Lib
open Core

let isNotNull = isNull >> not
let getAllFunctions (asm:Assembly) =
    asm.GetTypes()
    |> Array.collect (fun x->x.GetMembers())
    |> Array.choose (function :? MethodInfo as x -> Some x | _ -> None)
    |> Array.map (fun x->x, x.GetCustomAttribute(typeof<ExportAttribute>))
    |> Array.filter (snd >> isNotNull)
    |> Array.map (fun (f, att) -> (att :?> ExportAttribute).Name, f)
    |> Array.map (fun (name, f)-> { Name = name; MethodInfo = f}, f)


let funs =
    getAllFunctions typeof<Calc.Lib.ExportAttribute>.Assembly
    |> Array.map (fst >> (fun x-> x.Name, x))
    |> Map.ofArray


let refs =
    [ { Name = "i1"; Type = Integer }
      { Name = "i2"; Type = Integer }
      { Name = "s1"; Type = String }
      { Name = "s2"; Type = String }
      { Name = "b1"; Type = Boolean }
      { Name = "b2"; Type = Boolean }
      { Name = "d1"; Type = Decimal }
      { Name = "d2"; Type = Decimal } ]
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

let del = 
    "IF(1.0>2, 1,2)" 
    |> Tokenizer.tokenize
    |> Analyse.analyse
    |> TypeChecker.toTypedSyntaxTree funs refs
    |> Result.unwrap
    |> Emitter.generateMethod<int> funs
del.Invoke accessor |> printfn "%O"

