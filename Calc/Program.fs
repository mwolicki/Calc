#if INTERACTIVE
#r @"..\Calc.Lib\bin\Release\Calc.Lib.dll"
#load "Result.fs"
#load "Tokenizer.fs"
#load "Analyser.fs"
#load "TypeChecker.fs"
#load "Emitter.fs"
#endif

module Compile
open TypeChecker
open Core.Result

open System.Reflection
open Calc.Lib
open Core

let isNotNull (a:'a) = a |> (isNull >> not)
let getAllFunctions (asm:Assembly) =
    asm.GetTypes()
    |> Array.collect (fun x->x.GetMembers())
    |> Array.choose (function :? MethodInfo as x -> Some x | _ -> None)
    |> Array.map (fun x->x, x.GetCustomAttribute(typeof<ExportAttribute>))
    |> Array.filter (snd >> isNotNull)
    |> Array.map (fun (f, att) -> (att :?> ExportAttribute).Name, f)
    |> Array.map (fun (name, f)-> { Name = name; MethodInfo = f}, f)


let defaultFuncs =
    getAllFunctions typeof<Calc.Lib.ExportAttribute>.Assembly
    |> Array.map (fst >> (fun x-> x.Name, x))
    |> Map.ofArray

open System.Collections.Generic

let dictToMap (dict:IReadOnlyDictionary<_,_>) =
    dict |> Seq.map(fun (KeyValue kv) -> kv) |> Map.ofSeq

let compile'<'a> funcs refs code = 
    let emitter x =  Emitter.generateDynamicType funcs x :?> System.Func<IReferenceAccessor, 'a>
    Tokenizer.tokenize code
    |> Analyse.analyse
    |> TypeChecker.toTypedSyntaxTree funcs refs
    ==> emitter
    |> Result.unwrap

let compile<'a> funcs refs code = 
    //let funcs = dictToMap funcs
    let refs = dictToMap refs
    Tokenizer.tokenize code
    |> Analyse.analyse
    |> TypeChecker.toTypedSyntaxTree funcs refs
    ==> Emitter.generateDynamicType funcs
