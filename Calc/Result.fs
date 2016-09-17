module Core

type Result<'a, 'b> =
    | OK of 'a
    | Error of 'b


module Result = 
    let unwrap = function
    | OK x -> x
    | Error e -> failwithf "error = %A" e

    let unwrapError = function
    | OK _ as x -> failwithf "not expected value = %A" x
    | Error e -> e

    let map f = function
    | OK x -> f x |> OK
    | Error e -> Error e

    let(==>) a b = map b a
