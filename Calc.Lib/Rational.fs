namespace Calc.Lib

[<Struct>]
type Rational =

    val private a : int
    val private b : int
    
    member rat.GetValue() = decimal rat.a / decimal rat.b
    
    static member (+) (a:Rational, b:Rational) = Rational (a.a * b.b + b.a * a.b, a.b * b.b)
    static member (-) (a:Rational, b:Rational) = Rational (a.a * b.b - b.a * a.b, a.b * b.b)
    static member (*) (a:Rational, b:Rational) = Rational (a.a * b.a, a.b * b.b)
    static member (/) (a:Rational, b:Rational) = Rational (a.a * b.b, a.b * b.a)
    static member op_GreaterThan (a:Rational, b:Rational) = a.a * b.b > b.a * a.b
    static member op_GreaterThanOrEqual (a:Rational, b:Rational) = a.a * b.b >= b.a * a.b
    static member op_LessThan (a:Rational, b:Rational) = a.a * b.b < b.a * a.b
    static member op_LessThanOrEqual (a:Rational, b:Rational) = a.a * b.b <= b.a * a.b
    static member op_Equality (a:Rational, b:Rational) = a.Equals b
    static member op_Inequality (a:Rational, b:Rational) = a.Equals b |> not

    static member private Normalize a b =
        let d = Lib.gcd a b
        let b = b/d
        if b < 0 then -a/d, -b
        else a/d, b
        
    new (a',b') =
        let a', b' = Rational.Normalize a' b'
        { a = a'; b = b' }
        
    new (a':int) = { a = a'; b = 1 }
        
    override rat.ToString() =
        if rat.b = 1 then sprintf "%i" rat.a
        elif rat.a = 0 then "0"
        else sprintf "%i/%i" rat.a rat.b