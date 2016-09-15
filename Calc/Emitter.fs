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

    let getDecimalBits d =
        let bits = System.Decimal.GetBits d

        let scale = bits.[3] >>> 16
        let isNegative = if bits.[3] &&& 0x80000000 <> 0 then 1 else 0
        [bits.[0]; bits.[1]; bits.[2]; isNegative; scale;]

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


let emitInt (il:ILGenerator) = function
    | 0 -> il.Emit OpCodes.Ldc_I4_0
    | 1 -> il.Emit OpCodes.Ldc_I4_1
    | 2 -> il.Emit OpCodes.Ldc_I4_2
    | 3 -> il.Emit OpCodes.Ldc_I4_3
    | 4 -> il.Emit OpCodes.Ldc_I4_4
    | 5 -> il.Emit OpCodes.Ldc_I4_5
    | 6 -> il.Emit OpCodes.Ldc_I4_6
    | 7 -> il.Emit OpCodes.Ldc_I4_7
    | 8 -> il.Emit OpCodes.Ldc_I4_8
    | v when v >= -128 && v<= 127
        -> il.Emit (OpCodes.Ldc_I4_S, int8 v)
    | v -> il.Emit (OpCodes.Ldc_I4, v)

let generateMethod (fs:Map<FunName, FunDef>) (expr:TypedExpr) (il:ILGenerator) =
    let (|IsSimpleOperation|_|) = function
    | TOperatorCall (op, lhs, rhs, _) when 
        (lhs.Type = Integer && rhs.Type = Integer)
        || (lhs.Type = Boolean && rhs.Type = Boolean) ->
        match op with
        | Tokenizer.operator.Concat 
        | Tokenizer.operator.Plus -> [OpCodes.Add]
        | Tokenizer.operator.Minus -> [OpCodes.Sub]
        | Tokenizer.operator.Divide -> [OpCodes.Div]
        | Tokenizer.operator.Multiply -> [OpCodes.Mul]
        | Tokenizer.operator.Equals -> [OpCodes.Ceq]
        | Tokenizer.operator.Inequals -> [OpCodes.Ceq; OpCodes.Ldc_I4_0; OpCodes.Ceq]
        | Tokenizer.operator.Greater -> [OpCodes.Cgt]
        | Tokenizer.operator.GreaterOrEqual -> [OpCodes.Clt; OpCodes.Ldc_I4_0; OpCodes.Ceq]
        | Tokenizer.operator.Less -> [OpCodes.Clt]
        | Tokenizer.operator.LessOrEqual -> [OpCodes.Cgt; OpCodes.Ldc_I4_0; OpCodes.Ceq]
        |> fun x-> Some(x, lhs, rhs)
    | _ -> None

    let rec ilBuild (expr:TypedExpr) = 
        let emitInt = emitInt il
        match expr with
        | TConstBool true -> emitInt 1
        | TConstBool false -> emitInt 2
        | TConstNum type' ->
            match type' with
            | Tokenizer.number.Integer v -> emitInt v
            | Tokenizer.number.Real v -> 
                Numbers.getDecimalBits v
                |> Seq.iter emitInt
                let ctor = typeof<System.Decimal>.GetConstructor (BindingFlags.Instance ||| BindingFlags.Public, null, [|typeof<int>;typeof<int>;typeof<int>;typeof<bool>;typeof<byte>|], null)
                il.Emit (OpCodes.Newobj, ctor)

        | TConstStr s -> il.Emit(OpCodes.Ldstr, s)
        | TGroup expr -> ilBuild expr
        | TNegate expr when expr.Type = TypeChecker.Integer -> 
            ilBuild expr
            il.Emit OpCodes.Neg
        | TNegate expr when expr.Type = TypeChecker.Decimal -> 
            ilBuild <| TOperatorCall(Tokenizer.Multiply, TConstNum(Tokenizer.number.Real(-1M)), expr, Type.Decimal)
        | IsSimpleOperation (opCodes, lhs, rhs) ->
            ilBuild lhs
            ilBuild rhs
            List.iter il.Emit opCodes
        | TOperatorCall (opCode, lhs, rhs, t) ->
            let mi =
                match opCode with
                | Tokenizer.operator.Plus -> "op_Addition"
                | Tokenizer.operator.Minus -> "op_Subtraction"
                | Tokenizer.operator.Divide ->"op_Division"
                | Tokenizer.operator.Multiply -> "op_Multiply"
                | Tokenizer.operator.Equals -> "op_Equality"
                | Tokenizer.operator.Greater -> "op_GreaterThan"
                | Tokenizer.operator.Less -> "op_LessThan"
                | Tokenizer.operator.Inequals -> "op_Inequality"
                | Tokenizer.operator.GreaterOrEqual -> "op_GreaterThanOrEqual"
                | Tokenizer.operator.LessOrEqual -> "op_LessThanOrEqual"
                | Tokenizer.operator.Concat -> "Concat"
            let mi = lhs.Type.GetBCLType.GetMethod (mi, [|lhs.Type.GetBCLType; rhs.Type.GetBCLType|])
            ilBuild lhs
            ilBuild rhs
            il.EmitCall(OpCodes.Call, mi, null)

        | TReference (name, type') -> 
            let methodInfo = 
                match type' with
                | String -> "GetString"
                | Integer -> "GetInt"
                | Decimal -> "GetDecimal"
                | Boolean -> "GetBoolean"
                | x -> failwithf "Not supported yet type (%A) of reference" x
                |> typeof<IReferenceAccessor>.GetMethod
            il.Emit OpCodes.Ldarg_0
            il.Emit(OpCodes.Ldstr, name)
            il.EmitCall(OpCodes.Callvirt, methodInfo, null)
        | TFunctionCall (name, _, params') -> 
            params' |> List.iter ilBuild
            il.EmitCall(OpCodes.Call, fs.[name].MethodInfo, null)
        | TConvertType (currentType, newType, expr) ->
            ilBuild expr
            match currentType, newType with
            | currentType, String -> 
                let local = il.DeclareLocal currentType.GetBCLType
                il.Emit(OpCodes.Stloc, local);
                il.Emit(OpCodes.Ldloca, local);
                let mi = currentType.GetBCLType.GetMethod ("ToString", [||])
                il.EmitCall(OpCodes.Call, mi, null)
            | Integer, Decimal ->
                let ctor = typeof<System.Decimal>.GetConstructor (BindingFlags.Instance ||| BindingFlags.Public, null, [|typeof<int>|], null)
                il.Emit (OpCodes.Newobj, ctor)
            | _ -> failwithf "Conversion between %A & %A is not supported" currentType newType

        | TOperatorCall _
            -> failwithf "Unsupported expression %A" expr
    ilBuild expr
    il.Emit OpCodes.Ret
   

let generateDynamicType<'a> (fs:Map<FunName, FunDef>) (expr:TypedExpr) =
    let assemblyNumber = Numbers.getAssemblyNumber()
    let assemblyName = sprintf "emit-calc-%i.dll" assemblyNumber
    let builder = AssemblyBuilder.DefineDynamicAssembly(AssemblyName assemblyName, AssemblyBuilderAccess.RunAndSave)
    
    let m = builder.DefineDynamicModule (assemblyName, true)

    let typeBulder = m.DefineType ("emmit-type", staticClassAttributes)
    
    let name = Numbers.getMethodNumber() |> sprintf "method-%i" 
    let methodBuilder =typeBulder.DefineMethod(name, staticMethodAttributes, expr.Type.GetBCLType, [|typeof<IReferenceAccessor>|])

    methodBuilder.GetILGenerator() |> generateMethod fs expr

    let type' = typeBulder.CreateType()


//    builder.Save assemblyName

    let mi = type'.GetMethod name
    mi.CreateDelegate(typeof<System.Func<IReferenceAccessor, 'a>>) :?> System.Func<IReferenceAccessor, 'a>
