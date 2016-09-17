namespace Calc.Lib

open System.Runtime.InteropServices

type ExportAttribute(name) =
    inherit System.Attribute () 
    member __.Name : string = name