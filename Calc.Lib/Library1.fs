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

    [<Export("COS")>]
    let cos (a:decimal) = Math.Cos (float a) |> decimal
    
    [<Export("SIN")>]
    let sin (a:decimal) = Math.Sin (float a) |> decimal

    [<Export("ABS")>]
    let abs (a:decimal) = Math.Abs (float a) |> decimal

    [<Export("TEXT")>]
    let text (a:string) = a

    [<Export("STARTSWITH")>]
    let startsWith (s:string) (pattern:string) = s.StartsWith (pattern, StringComparison.InvariantCulture) 

    [<Export("ACOS")>]
    let acos (a:decimal) = Math.Acos (float a) |> decimal

    [<Export("ASIN")>]
    let asin (a:decimal) = Math.Asin (float a) |> decimal

    [<Export("ATAN")>]
    let atan (a:decimal) = Math.Atan (float a) |> decimal

    [<Export("ATAN2")>]
    let atan2 (a:decimal) (b:decimal) = Math.Atan2 (float a, float b) |> decimal

    [<Export("CEILING")>]
    let ceiling (a:decimal) = Math.Ceiling (float a) |> decimal

    [<Export("COSH")>]
    let cosh (a:decimal) = Math.Cosh (float a) |> decimal

    [<Export("FLOOR")>]
    let floor (a:decimal) = Math.Floor (float a) |> decimal

    [<Export("TAN")>]
    let tan (a:decimal) = Math.Tan (float a) |> decimal

    [<Export("SINH")>]
    let sinh (a:decimal) = Math.Sinh (float a) |> decimal

    [<Export("TANH")>]
    let tanh (a:decimal) = Math.Tanh (float a) |> decimal

    [<Export("ROUND")>]
    let round (a:decimal) = Math.Round (float a) |> decimal

    [<Export("TRUNCATE")>]
    let truncate (a:decimal) = Math.Truncate (float a) |> decimal

    [<Export("SQRT")>]
    let sqrt (a:decimal) = Math.Sqrt (float a) |> decimal

    [<Export("LOG")>]
    let log (a:decimal) = Math.Log (float a) |> decimal

    [<Export("LOG10")>]
    let log10 (a:decimal) = Math.Log10 (float a) |> decimal

    [<Export("EXP")>]
    let exp (a:decimal) = Math.Exp (float a) |> decimal

    [<Export("POW")>]
    let pow (a:decimal) (b:decimal) = Math.Pow (float a, float b) |> decimal

    [<Export("IEEEREMAINDER")>]
    let ieeeremainder (a:decimal) (b:decimal) = Math.IEEERemainder (float a, float b) |> decimal

    [<Export("MAX")>]
    let max (a:decimal) (b:decimal) = Math.Max (float a, float b) |> decimal

    [<Export("MIN")>]
    let min (a:decimal) (b:decimal) = Math.Min (float a, float b) |> decimal

    [<Export("SIGN")>]
    let sign (a:decimal) = Math.Sign (float a) |> decimal

    [<Export("FORMAT")>]
    let format a (b:string) = String.Format (a, b)

    [<Export("ISNULLORWHITESPACE")>]
    let isnullorwhitespace a = String.IsNullOrWhiteSpace a

    [<Export("ISNULLOREMPTY")>]
    let isnullorempty a = String.IsNullOrEmpty a
    
    [<Export("NOW")>]
    let now () = System.DateTime.Now.ToString()
    
    [<Export("TODAY")>]
    let time () = System.DateTime.Now.ToShortDateString()    