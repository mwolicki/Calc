module Core

type Result<'a, 'b> =
    | OK of 'a
    | Error of 'b

let unwrap = function
| OK x -> x
| Error e -> failwithf "error = %A" e