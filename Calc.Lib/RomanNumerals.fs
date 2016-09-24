namespace Calc.Lib

[<Struct>]
type RomanNum =
    val Number : int
    static member private values =
         Map [ 'I', 1
               'V', 5
               'X', 10
               'L', 50
               'C', 100
               'D', 500
               'M', 1000]

    static member TryParse(s:string) =
         let rec parse' (prev:int ) (currVal:int) (str:int list) : int =
            match str with
            | []-> currVal
            | n :: chs ->
                match n, prev with 
                | 5, 1
                | 10, 1
                | 50, 10
                | 100, 10
                | 500, 100
                | 1000, 100 -> parse' n (currVal + n -  prev * 2) chs
                | _ -> parse' n (currVal + n) chs

         if not <| System.String.IsNullOrEmpty s && s |> Seq.forall RomanNum.values.ContainsKey then
            s 
            |> Seq.choose RomanNum.values.TryFind
            |> Seq.toList
            |> parse' 0 0
            |> Some
         else None

    new (number) = { Number = number }
        
    override r.ToString() = r.Number.ToString()
