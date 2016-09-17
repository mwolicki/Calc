namespace Calc.Lib

type IReferenceAccessor =
    abstract member GetInt : string -> int
    abstract member GetString : string -> string
    abstract member GetBoolean : string -> bool
    abstract member GetDecimal : string -> decimal

module Lib = 
    open System

    [<Export("IF")>]
    let ifFunc x (a:string) b = if x then a else b

    [<Export("PRINT")>]
    let print a= printfn "%s" a

    [<Export("COS")>]
    let cos (a:decimal):decimal = Math.Cos (float a) |> decimal
    
    [<Export("SIN")>]
    let sin (a:decimal) = Math.Sin (float a) |> decimal

    [<Export("TEXT")>]
    let text (a:string) = a


    [<Export("STARTSWITH")>]
    let startsWith (s:string) (pattern:string) = s.StartsWith (pattern, StringComparison.InvariantCulture) 

