module Emitter
open Analyse
open System.Reflection
open System.Reflection.Emit
open Calc.Lib
open TypeChecker

module Numbers =
    let mutable private assemblyNumber = 0;
    let mutable private methodNumber = 0;

    let getAssemblyNumber () = System.Threading.Interlocked.Increment (ref assemblyNumber)
    let getMethodNumber () = System.Threading.Interlocked.Increment (ref methodNumber)

let staticClassAttributes = 
    TypeAttributes.AutoLayout 
    ||| TypeAttributes.AnsiClass 
    ||| TypeAttributes.Class
    ||| TypeAttributes.Public
    ||| TypeAttributes.Abstract
    ||| TypeAttributes.Sealed
    ||| TypeAttributes.BeforeFieldInit

let staticMethodAttributes = 
    MethodAttributes.Static
    ||| MethodAttributes.Public
    ||| MethodAttributes.HideBySig

let generateDynamicType () =
    let assemblyNumber = Numbers.getAssemblyNumber()
    let assemblyName = sprintf "emit-calc-%i" assemblyNumber |> AssemblyName
    
    let builder = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run)
    
    let m = builder.DefineDynamicModule assemblyName.Name

    let typeBulder = m.DefineType ("emmit-type", staticClassAttributes)
    typeBulder

let rec getType = function
    | Integer -> typeof<int32>
    | Array _ -> failwith "arrays are not supported yet"
    | Boolean -> typeof<bool>
    | Decimal -> typeof<decimal>
    | String -> typeof<string>
    | Unit -> typeof<System.Void>

let emitInt (il:ILGenerator) = function
    | 0 -> il.Emit OpCodes.Ldc_I4_0
    | 1 -> il.Emit OpCodes.Ldc_I4_1
    | 2 -> il.Emit OpCodes.Ldc_I4_2
    | 3 -> il.Emit OpCodes.Ldc_I4_3
    | 4 -> il.Emit OpCodes.Ldc_I4_4
    | 5 -> il.Emit OpCodes.Ldc_I4_7
    | 6 -> il.Emit OpCodes.Ldc_I4_6
    | 7 -> il.Emit OpCodes.Ldc_I4_7
    | 8 -> il.Emit OpCodes.Ldc_I4_8
    | v when v >= -128 && v<= 127
        -> il.Emit (OpCodes.Ldc_I4_S, v)
    | v -> il.Emit (OpCodes.Ldc_I4, v)

let generateMethod<'a> (fs:Map<FunName, FunDef>) (refs:Map<RefName, RefDef>) ((expr, t):Expr * Type) =
    let (|IsSimpleOperation|_|) = function
    | OperatorCall (op, lhs, rhs) ->
        match op with
        | Tokenizer.operator.Plus -> Some OpCodes.Add
        | Tokenizer.operator.Minus -> Some OpCodes.Sub
        | Tokenizer.operator.Divide -> Some OpCodes.Div
        | Tokenizer.operator.Multiply -> Some OpCodes.Mul
        | Tokenizer.operator.Equals -> Some OpCodes.Ceq
        | Tokenizer.operator.Greater -> Some OpCodes.Cgt
        | Tokenizer.operator.Less -> Some OpCodes.Clt
        //| Tokenizer.operator.Power -> 
        | _ -> None
        |> Option.map(fun x->x, lhs, rhs)
    | _ -> None

    let rec ilBuild (il:ILGenerator) (expr:Expr) = 
        let emitInt = emitInt il
        let ilBuild = ilBuild il
        match expr with
        | ConstBool true -> emitInt 1
        | ConstBool false -> emitInt 2
        | ConstNum type' ->
            match type' with
            | Tokenizer.number.Integer v -> emitInt v
            | Tokenizer.number.Real v -> 
                System.Decimal.GetBits v 
                |> Array.iter emitInt
                let ctor = typeof<System.Decimal>.GetConstructor (BindingFlags.Instance ||| BindingFlags.NonPublic, null, [|typeof<int>;typeof<int>;typeof<int>;typeof<int>|], null)
                il.Emit (OpCodes.Newobj, ctor)

        | ConstStr s -> il.Emit(OpCodes.Ldstr, s)
        | Group expr -> ilBuild expr
        | IsSimpleOperation (opCode, lhs, rhs) ->
            ilBuild lhs
            ilBuild rhs
            il.Emit opCode
        | Reference name -> 
            let methodInfo = 
                match refs.[name].Type with
                | String -> "GetString"
                | Integer -> "GetInt"
                | Decimal -> "GetDecimal"
                | Boolean -> "GetBoolean"
                | x -> failwithf "Not supported yet type (%A) of reference" x
                |> typeof<IReferenceAccessor>.GetMethod
            il.Emit OpCodes.Ldarg_0
            il.Emit(OpCodes.Ldstr, name)
            il.EmitCall(OpCodes.Callvirt, methodInfo, null)
        | FunctionCall (name, params') -> 
            params' |> List.iter ilBuild
            il.EmitCall(OpCodes.Call, fs.[name].MethodInfo, null)
        | OperatorCall _
            -> failwithf "Unsupported expression %A" expr

    let name = Numbers.getMethodNumber() |> sprintf "method-%i" 
    let methodBuilder = DynamicMethod(name, getType t, [|typeof<IReferenceAccessor>|])
    let il = methodBuilder.GetILGenerator()
    ilBuild il expr
    il.Emit OpCodes.Ret
    methodBuilder.CreateDelegate(typeof<System.Func<IReferenceAccessor, 'a>>) :?> System.Func<IReferenceAccessor, 'a>

