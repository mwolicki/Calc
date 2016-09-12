namespace Calc.Lib

type ExportAttribute(name) =
    inherit System.Attribute () 
    member __.Name : string = name
