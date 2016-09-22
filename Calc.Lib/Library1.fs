namespace Calc.Lib

[<Struct; CustomEquality; NoComparison>]
type Date =
    val private DateTime : System.DateTime

    member date.Year = date.DateTime.Year
    member date.Month = date.DateTime.Month
    member date.Day = date.DateTime.Day

    override date.ToString() = date.DateTime.ToShortDateString()
    override date.Equals o =
        match o with
        | :? Date as s -> date.DateTime.Equals s.DateTime
        | _ -> false
    override date.GetHashCode () = date.DateTime.GetHashCode ()
    new (year, month, day) = { DateTime = System.DateTime(year, month, day) }
    new (dt:System.DateTime) = { DateTime =  System.DateTime(dt.Year, dt.Month, dt.Day) }



type IReferenceAccessor =
    abstract member GetInt : string -> int
    abstract member GetString : string -> string
    abstract member GetBoolean : string -> bool
    abstract member GetDecimal : string -> decimal
    abstract member GetDateTime : string -> System.DateTime
    abstract member GetDate: string -> Date




module Lib = 
    open System

    [<Export("GCD")>]
    let rec gcd a b = 
        if b = 0 then a
        else gcd b (a % b)

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
    let pow (a:decimal) (b:decimal) =
        Math.Pow (float a, float b) |> decimal

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
    let now () = System.DateTime.Now
    
    [<Export("TODAY")>]
    let time () = System.DateTime.Now |> Date

module Financial = 
    open Excel.FinancialFunctions

    [<Export("ACCRINT")>]
    let accrint (issue, firstinterest, settlement, rate:decimal, par:decimal, frequency, basis, calcmethod) = Financial.AccrInt (issue, firstinterest, settlement, float rate, float par, enum<Frequency> frequency, enum<DayCountBasis> basis, enum<AccrIntCalcMethod> calcmethod) |> decimal

    [<Export("ACCRINT2")>]
    let accrint2 (issue, firstinterest, settlement, rate:decimal, par:decimal, frequency, basis) = Financial.AccrInt (issue, firstinterest, settlement, float rate, float par, enum<Frequency> frequency, enum<DayCountBasis> basis) |> decimal

    [<Export("ACCRINTM")>]
    let accrintm (issue, settlement, rate:decimal, par:decimal, basis) = Financial.AccrIntM (issue, settlement, float rate, float par, enum<DayCountBasis> basis) |> decimal

    [<Export("AMORDEGRC")>]
    let amordegrc (cost:decimal, datepurchased, firstperiod, salvage:decimal, period:decimal, rate:decimal, basis, excelcompliant) = Financial.AmorDegrc (float cost, datepurchased, firstperiod, float salvage, float period, float rate, enum<DayCountBasis> basis, excelcompliant) |> decimal

    [<Export("AMORLINC")>]
    let amorlinc (cost:decimal, datepurchased, firstperiod, salvage:decimal, period:decimal, rate:decimal, basis) = Financial.AmorLinc (float cost, datepurchased, firstperiod, float salvage, float period, float rate, enum<DayCountBasis> basis) |> decimal

    [<Export("COUPDAYSBS")>]
    let coupdaysbs (settlement, maturity, frequency, basis) = Financial.CoupDaysBS (settlement, maturity, enum<Frequency> frequency, enum<DayCountBasis> basis) |> decimal

    [<Export("COUPDAYS")>]
    let coupdays (settlement, maturity, frequency, basis) = Financial.CoupDays (settlement, maturity, enum<Frequency> frequency, enum<DayCountBasis> basis) |> decimal

    [<Export("COUPDAYSNC")>]
    let coupdaysnc (settlement, maturity, frequency, basis) = Financial.CoupDaysNC (settlement, maturity, enum<Frequency> frequency, enum<DayCountBasis> basis) |> decimal

    [<Export("COUPNCD")>]
    let coupncd (settlement, maturity, frequency, basis) = Financial.CoupNCD (settlement, maturity, enum<Frequency> frequency, enum<DayCountBasis> basis)

    [<Export("COUPNUM")>]
    let coupnum (settlement, maturity, frequency, basis) = Financial.CoupNum (settlement, maturity, enum<Frequency> frequency, enum<DayCountBasis> basis) |> decimal

    [<Export("COUPPCD")>]
    let couppcd (settlement, maturity, frequency, basis) = Financial.CoupPCD (settlement, maturity, enum<Frequency> frequency, enum<DayCountBasis> basis)

    [<Export("CUMIPMT")>]
    let cumipmt (rate:decimal, nper:decimal, pv:decimal, startperiod:decimal, endperiod:decimal, typ) = Financial.CumIPmt (float rate, float nper, float pv, float startperiod, float endperiod, enum<PaymentDue> typ) |> decimal

    [<Export("CUMPRINC")>]
    let cumprinc (rate:decimal, nper:decimal, pv:decimal, startperiod:decimal, endperiod:decimal, typ) = Financial.CumPrinc (float rate, float nper, float pv, float startperiod, float endperiod, enum<PaymentDue> typ) |> decimal

    [<Export("DB")>]
    let db (cost:decimal, salvage:decimal, life:decimal, period:decimal, month:decimal) = Financial.Db (float cost, float salvage, float life, float period, float month) |> decimal

    [<Export("DB2")>]
    let db2 (cost:decimal, salvage:decimal, life:decimal, period:decimal) = Financial.Db (float cost, float salvage, float life, float period) |> decimal

    [<Export("DDB")>]
    let ddb (cost:decimal, salvage:decimal, life:decimal, period:decimal, factor:decimal) = Financial.Ddb (float cost, float salvage, float life, float period, float factor) |> decimal

    [<Export("DDB2")>]
    let ddb2 (cost:decimal, salvage:decimal, life:decimal, period:decimal) = Financial.Ddb (float cost, float salvage, float life, float period) |> decimal

    [<Export("DISC")>]
    let disc (settlement, maturity, pr:decimal, redemption:decimal, basis) = Financial.Disc (settlement, maturity, float pr, float redemption, enum<DayCountBasis> basis) |> decimal

    [<Export("DOLLARDE")>]
    let dollarde (fractionaldollar:decimal, fraction:decimal) = Financial.DollarDe (float fractionaldollar, float fraction) |> decimal

    [<Export("DOLLARFR")>]
    let dollarfr (decimaldollar:decimal, fraction:decimal) = Financial.DollarFr (float decimaldollar, float fraction) |> decimal

    [<Export("DURATION")>]
    let duration (settlement, maturity, coupon:decimal, yld:decimal, frequency, basis) = Financial.Duration (settlement, maturity, float coupon, float yld, enum<Frequency> frequency, enum<DayCountBasis> basis) |> decimal

    [<Export("EFFECT")>]
    let effect (nominalrate:decimal, npery:decimal) = Financial.Effect (float nominalrate, float npery) |> decimal

    [<Export("FV")>]
    let fv (rate:decimal, nper:decimal, pmt:decimal, pv:decimal, typ) = Financial.Fv (float rate, float nper, float pmt, float pv, enum<PaymentDue> typ) |> decimal

    [<Export("FVSCHEDULE")>]
    let fvschedule (principal:decimal, schedule) = Financial.FvSchedule (float principal, schedule) |> decimal

    [<Export("INTRATE")>]
    let intrate (settlement, maturity, investment:decimal, redemption:decimal, basis) = Financial.IntRate (settlement, maturity, float investment, float redemption, enum<DayCountBasis> basis) |> decimal

    [<Export("IPMT")>]
    let ipmt (rate:decimal, per:decimal, nper:decimal, pv:decimal, fv:decimal, typ) = Financial.IPmt (float rate, float per, float nper, float pv, float fv, enum<PaymentDue> typ) |> decimal

    [<Export("IRR")>]
    let irr (values, guess:decimal) = Financial.Irr (values, float guess) |> decimal

    [<Export("IRR2")>]
    let irr2 (values) = Financial.Irr (values) |> decimal

    [<Export("ISPMT")>]
    let ispmt (rate:decimal, per:decimal, nper:decimal, pv:decimal) = Financial.ISPmt (float rate, float per, float nper, float pv) |> decimal

    [<Export("MDURATION")>]
    let mduration (settlement, maturity, coupon:decimal, yld:decimal, frequency, basis) = Financial.MDuration (settlement, maturity, float coupon, float yld, enum<Frequency> frequency, enum<DayCountBasis> basis) |> decimal

    [<Export("MIRR")>]
    let mirr (values, financerate:decimal, reinvestrate:decimal) = Financial.Mirr (values, float financerate, float reinvestrate) |> decimal

    [<Export("NOMINAL")>]
    let nominal (effectrate:decimal, npery:decimal) = Financial.Nominal (float effectrate, float npery) |> decimal

    [<Export("NPER")>]
    let nper (rate:decimal, pmt:decimal, pv:decimal, fv:decimal, typ) = Financial.NPer (float rate, float pmt, float pv, float fv, enum<PaymentDue> typ) |> decimal

    [<Export("NPV")>]
    let npv (rate:decimal, values) = Financial.Npv (float rate, values) |> decimal

    [<Export("ODDFPRICE")>]
    let oddfprice (settlement, maturity, issue, firstcoupon, rate:decimal, yld:decimal, redemption:decimal, frequency, basis) = Financial.OddFPrice (settlement, maturity, issue, firstcoupon, float rate, float yld, float redemption, enum<Frequency> frequency, enum<DayCountBasis> basis) |> decimal

    [<Export("ODDFYIELD")>]
    let oddfyield (settlement, maturity, issue, firstcoupon, rate:decimal, pr:decimal, redemption:decimal, frequency, basis) = Financial.OddFYield (settlement, maturity, issue, firstcoupon, float rate, float pr, float redemption, enum<Frequency> frequency, enum<DayCountBasis> basis) |> decimal

    [<Export("ODDLPRICE")>]
    let oddlprice (settlement, maturity, lastinterest, rate:decimal, yld:decimal, redemption:decimal, frequency, basis) = Financial.OddLPrice (settlement, maturity, lastinterest, float rate, float yld, float redemption, enum<Frequency> frequency, enum<DayCountBasis> basis) |> decimal

    [<Export("ODDLYIELD")>]
    let oddlyield (settlement, maturity, lastinterest, rate:decimal, pr:decimal, redemption:decimal, frequency, basis) = Financial.OddLYield (settlement, maturity, lastinterest, float rate, float pr, float redemption, enum<Frequency> frequency, enum<DayCountBasis> basis) |> decimal

    [<Export("PMT")>]
    let pmt (rate:decimal, nper:decimal, pv:decimal, fv:decimal, typ) = Financial.Pmt (float rate, float nper, float pv, float fv, enum<PaymentDue> typ) |> decimal

    [<Export("PPMT")>]
    let ppmt (rate:decimal, per:decimal, nper:decimal, pv:decimal, fv:decimal, typ) = Financial.PPmt (float rate, float per, float nper, float pv, float fv, enum<PaymentDue> typ) |> decimal

    [<Export("PRICE")>]
    let price (settlement, maturity, rate:decimal, yld:decimal, redemption:decimal, frequency, basis) = Financial.Price (settlement, maturity, float rate, float yld, float redemption, enum<Frequency> frequency, enum<DayCountBasis> basis) |> decimal

    [<Export("PRICEDISC")>]
    let pricedisc (settlement, maturity, discount:decimal, redemption:decimal, basis) = Financial.PriceDisc (settlement, maturity, float discount, float redemption, enum<DayCountBasis> basis) |> decimal

    [<Export("PRICEMAT")>]
    let pricemat (settlement, maturity, issue, rate:decimal, yld:decimal, basis) = Financial.PriceMat (settlement, maturity, issue, float rate, float yld, enum<DayCountBasis> basis) |> decimal

    [<Export("PV")>]
    let pv (rate:decimal, nper:decimal, pmt:decimal, fv:decimal, typ) = Financial.Pv (float rate, float nper, float pmt, float fv, enum<PaymentDue> typ) |> decimal

    [<Export("RATE")>]
    let rate (nper:decimal, pmt:decimal, pv:decimal, fv:decimal, typ, guess:decimal) = Financial.Rate (float nper, float pmt, float pv, float fv, enum<PaymentDue> typ, float guess) |> decimal

    [<Export("RATE2")>]
    let rate2 (nper:decimal, pmt:decimal, pv:decimal, fv:decimal, typ) = Financial.Rate (float nper, float pmt, float pv, float fv, enum<PaymentDue> typ) |> decimal

    [<Export("RECEIVED")>]
    let received (settlement, maturity, investment:decimal, discount:decimal, basis) = Financial.Received (settlement, maturity, float investment, float discount, enum<DayCountBasis> basis) |> decimal

    [<Export("SLN")>]
    let sln (cost:decimal, salvage:decimal, life:decimal) = Financial.Sln (float cost, float salvage, float life) |> decimal

    [<Export("SYD")>]
    let syd (cost:decimal, salvage:decimal, life:decimal, per:decimal) = Financial.Syd (float cost, float salvage, float life, float per) |> decimal

    [<Export("TBILLEQ")>]
    let tbilleq (settlement, maturity, discount:decimal) = Financial.TBillEq (settlement, maturity, float discount) |> decimal

    [<Export("TBILLPRICE")>]
    let tbillprice (settlement, maturity, discount:decimal) = Financial.TBillPrice (settlement, maturity, float discount) |> decimal

    [<Export("TBILLYIELD")>]
    let tbillyield (settlement, maturity, pr:decimal) = Financial.TBillYield (settlement, maturity, float pr) |> decimal

    [<Export("VDB")>]
    let vdb (cost:decimal, salvage:decimal, life:decimal, startperiod:decimal, endperiod:decimal, factor:decimal, noswitch) = Financial.Vdb (float cost, float salvage, float life, float startperiod, float endperiod, float factor, enum<VdbSwitch> noswitch) |> decimal

    [<Export("VDB2")>]
    let vdb2 (cost:decimal, salvage:decimal, life:decimal, startperiod:decimal, endperiod:decimal, factor:decimal) = Financial.Vdb (float cost, float salvage, float life, float startperiod, float endperiod, float factor) |> decimal

    [<Export("VDB3")>]
    let vdb3 (cost:decimal, salvage:decimal, life:decimal, startperiod:decimal, endperiod:decimal) = Financial.Vdb (float cost, float salvage, float life, float startperiod, float endperiod) |> decimal

    [<Export("XIRR")>]
    let xirr (values, dates, guess:decimal) = Financial.XIrr (values, dates, float guess) |> decimal

    [<Export("XIRR2")>]
    let xirr2 (values, dates) = Financial.XIrr (values, dates) |> decimal

    [<Export("XNPV")>]
    let xnpv (rate:decimal, values, dates) = Financial.XNpv (float rate, values, dates) |> decimal

    [<Export("YIELD")>]
    let yield' (settlement, maturity, rate:decimal, pr:decimal, redemption:decimal, frequency, basis) = Financial.Yield (settlement, maturity, float rate, float pr, float redemption, enum<Frequency> frequency, enum<DayCountBasis> basis) |> decimal

    [<Export("YIELDDISC")>]
    let yielddisc (settlement, maturity, pr:decimal, redemption:decimal, basis) = Financial.YieldDisc (settlement, maturity, float pr, float redemption, enum<DayCountBasis> basis) |> decimal

    [<Export("YIELDMAT")>]
    let yieldmat (settlement, maturity, issue, rate:decimal, pr:decimal, basis) = Financial.YieldMat (settlement, maturity, issue, float rate, float pr, enum<DayCountBasis> basis) |> decimal

    [<Export("YEARFRAC")>]
    let yearfrac (startdate, enddate, basis) = Financial.YearFrac (startdate, enddate, enum<DayCountBasis> basis) |> decimal


[<Struct>]
type Rational =

    val private a : int
    val private b : int
    
    member rat.GetValue() = decimal rat.a/ decimal rat.b
    
    static member (+) (a:Rational, b:Rational) = Rational (a.a * b.b + b.a * a.b, a.b * b.b)
    static member (-) (a:Rational, b:Rational) = Rational (a.a * b.b - b.a * a.b, a.b * b.b)
    static member (*) (a:Rational, b:Rational) = Rational (a.a * b.a, a.b * b.b)
    static member (/) (a:Rational, b:Rational) = Rational (a.a * b.b, a.b * b.a)

    static member private normalize a b =
        let d = Lib.gcd a b
        let b = b/d
        if b < 0 then -a/d, -b
        else a/d, b
        
    new (a',b') =
        let a', b' = Rational.normalize a' b'
        { a = a'; b = b' }
        
    new (a':Rational,b':Rational) =
        let a', b' = Rational.normalize (a'.a * b'.a) (a'.b * b'.b)
        { a = a'; b = b' }
        
    override rat.ToString() =
        if rat.b = 1 then sprintf "%i" rat.a
        elif rat.a = 0 then "0"
        else sprintf "%i/%i" rat.a rat.b