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
    let getType t= 
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
    let getReturnType (mi:MethodInfo) = mi.ReturnType |> getType
    let getParamsTypes (mi:MethodInfo) = 
        mi.GetParameters ()
        |> Array.map (fun p -> p.ParameterType |> getType)
        |> List.ofArray


    asm.GetTypes()
    |> Array.collect (fun x->x.GetMembers())
    |> Array.choose (function :? MethodInfo as x -> Some x | _ -> None)
    |> Array.map (fun x->x, x.GetCustomAttribute(typeof<ExportAttribute>))
    |> Array.filter (snd >> isNotNull)
    |> Array.map (fun (f, att) -> (att :?> ExportAttribute).Name, f)
    |> Array.map (fun (name, f)-> { Name = name; ReturnType = getReturnType f; Parameters = getParamsTypes f; MethodInfo = f}, f)


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
      member __.GetBoolean _ = false
      member __.GetString _ = "text"
      member __.GetDecimal _ = 2m }

let analyse = Tokenizer.tokenize >> Analyse.analyse

let typecheck = Tokenizer.tokenize >> Analyse.analyse >> (TypeChecker.check funs refs)

Tokenizer.tokenize "true=false"
Tokenizer.tokenize "((``ala ma kota``aaa,;  1 + 23.45 ``123``'' 'sdakjasdkas' TRUE )) <> ><=="

typecheck "true = false" 
typecheck "IF(i1+i2>0 ; COS(i2); 1 + i2^2 * 2^2)"


let del = 
    typecheck "IF(i1+i2<0 ; 'A'; 'B')" 
    |> unwrap
    |> Emitter.generateMethod<string> funs refs
del.Invoke accessor

