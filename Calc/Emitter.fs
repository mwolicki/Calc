module Emitter
open Analyse
open System.Reflection
open System.Reflection.Emit
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

let generateMethod<'a> ((expr, t):Expr * Type) =
    let (|IsSimpleOperation|_|) = function
    | OperatorCall (op, lhs, rhs) ->
        (match op with
        | Tokenizer.operator.Plus -> Some OpCodes.Add
        | Tokenizer.operator.Minus -> Some OpCodes.Sub
        | Tokenizer.operator.Divide -> Some OpCodes.Div
        | Tokenizer.operator.Multiply -> Some OpCodes.Mul
        | Tokenizer.operator.Equals -> Some OpCodes.Ceq
        | Tokenizer.operator.Greater -> Some OpCodes.Cgt
        | Tokenizer.operator.Less -> Some OpCodes.Clt
        //| Tokenizer.operator.Power -> 
        | _ -> None) |> Option.map(fun x->x, lhs, rhs)
    | _ -> None

    let rec ilBuild (il:ILGenerator) (expr:Expr) = 
        match expr with
        | ConstBool true -> il.Emit(OpCodes.Ldc_I4_1)
        | ConstBool false -> il.Emit(OpCodes.Ldc_I4_0)
        | ConstNum (Tokenizer.number.Integer v) ->
            il.Emit(OpCodes.Ldc_I4, v)
            printfn "%A %i (%A)" OpCodes.Ldc_I4 v expr
        | ConstStr s -> il.Emit(OpCodes.Ldstr, s)
        | Group expr -> ilBuild il expr
        | IsSimpleOperation (opCode, lhs, rhs) ->
            ilBuild il lhs
            ilBuild il rhs
            il.Emit opCode
            printfn "%A (%A)" opCode expr
        | OperatorCall _
        | FunctionCall _
        | ConstNum _ 
        | Reference _ 
            -> failwithf "Unsupported expression %A" expr

    let name = Numbers.getMethodNumber() |> sprintf "method-%i" 
    let methodBuilder = DynamicMethod(name, getType t, null)
    let il = methodBuilder.GetILGenerator()
    ilBuild il expr
    il.Emit(OpCodes.Ret)
    methodBuilder.CreateDelegate(typeof<System.Func<'a>>) :?> System.Func<'a>

