namespace Calc.Lib

[<Struct; CustomEquality; NoComparison>]
type Date =
    val private DateTime : System.DateTime

    member date.Year = date.DateTime.Year
    member date.Month = date.DateTime.Month
    member date.Day = date.DateTime.Day

    static member (+) (d:Date, timeSpan:System.TimeSpan) = d.DateTime + timeSpan
    static member (-) (d:Date, timeSpan:System.TimeSpan) = d.DateTime - timeSpan
    static member (-) (a:Date, b:Date) = a.DateTime - b.DateTime
    static member (-) (a:Date, b:System.DateTime) = a.DateTime - b

    static member op_GreaterThan (a:Date, b:Date) = a.DateTime > b.DateTime
    static member op_GreaterThanOrEqual (a:Date, b:Date) = a.DateTime >= b.DateTime
    static member op_LessThan (a:Date, b:Date) = a.DateTime < b.DateTime
    static member op_LessThanOrEqual (a:Date, b:Date) = a.DateTime <= b.DateTime
    static member op_Equality (a:Date, b:Date) = a.DateTime = b.DateTime
    static member op_Inequality (a:Date, b:Date) = a.DateTime <> b.DateTime

    override date.ToString() = date.DateTime.ToShortDateString()
    override date.Equals o =
        match o with
        | :? Date as s -> date.DateTime.Equals s.DateTime
        | _ -> false
    override date.GetHashCode () = date.DateTime.GetHashCode ()
    new (year, month, day) = { DateTime = System.DateTime(year, month, day) }
    new (dt:System.DateTime) = { DateTime =  System.DateTime(dt.Year, dt.Month, dt.Day) }
