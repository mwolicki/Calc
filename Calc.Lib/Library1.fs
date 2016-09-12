module Lib

open Calc.Lib
open System

[<Export("IF")>]
let ifFunc x (a:string) b = if x then a else b

[<Export("PRINT")>]
let print a = printfn "%s" a

[<Export("COS")>]
let cos = Math.Cos

[<Export("SIN")>]
let sin = Math.Sin

[<Export("STARTSWITH")>]
let startsWith (s:string) (pattern:string) = s.StartsWith (pattern, StringComparison.InvariantCulture) 

