namespace Calc.Lib

type ExportAttribute(name, referentialTransparent) =
    inherit System.Attribute () 
    member __.Name : string = name
    member __.IsReferentialTransparent = referentialTransparent
    new (name) = ExportAttribute(name, true)