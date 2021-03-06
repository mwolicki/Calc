﻿#if INTERACTIVE
#r @"..\Calc.Lib\bin\Release\Calc.Lib.dll"
#load "Result.fs"
#load "Tokenizer.fs"
#load "Analyser.fs"
#load "TypeChecker.fs"
#load "Emitter.fs"
#load "Eval.fs"
#load "Optimizer.fs"
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
        |> Array.map (fun (f, att) -> (att :?> ExportAttribute), f)
        |> Array.map (fun (att, f)-> { Name = att.Name; MethodInfo = f; IsReferntionalTransparent = att.IsReferentialTransparent}, f)


    let defaultFuncs =
        getAllFunctions typeof<Calc.Lib.ExportAttribute>.Assembly
        |> Array.map (fst >> (fun x-> x.Name, x))
        |> Map.ofArray

    open System.Collections.Generic

    let dictToMap (dict:IReadOnlyDictionary<_,_>) =
        dict |> Seq.map(fun (KeyValue kv) -> kv) |> Map.ofSeq

    let getAST code =
        Tokenizer.tokenize code
        |> Analyse.analyse
        |> Result.unwrap
    
    let getTypedExpr funcs refs code = 
        Tokenizer.tokenize code
        |> Analyse.analyse
        |> TypeChecker.toTypedSyntaxTree funcs refs

    let compile'<'a> funcs refs code = 
        let emitter x =  Emitter.generateDynamicType funcs x :?> System.Func<IReferenceAccessor, 'a>
        getTypedExpr funcs refs code
        ==> Optimizer.optimizer
        ==> emitter
        |> Result.unwrap

    let compile<'a> funcs refs code = 
        let refs = dictToMap refs
        getTypedExpr funcs refs code
        ==> Optimizer.optimizer
        ==> Emitter.generateDynamicType funcs