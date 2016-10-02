namespace Calc.Lib

type IReferenceAccessor =
    abstract member GetInt : string -> int
    abstract member GetString : string -> string
    abstract member GetBoolean : string -> bool
    abstract member GetDecimal : string -> decimal
    abstract member GetDateTime : string -> System.DateTime
    abstract member GetDate: string -> Date
    abstract member Get: string -> 'a

