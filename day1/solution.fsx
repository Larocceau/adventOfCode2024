open System.Collections.Generic

module Tuple =
    let map transformation (a, b) = transformation a, transformation b

let lists =
    "day1/input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map (
        List.ofSeq
        >> function
            | [ w1c1; w1c2; w1c3; w1c4; w1c5; ' '; ' '; ' '; w2c1; w2c2; w2c3; w2c4; w2c5 ] ->
                [| w1c1; w1c2; w1c3; w1c4; w1c5 |] |> System.String |> int,
                [| w2c1; w2c2; w2c3; w2c4; w2c5 |] |> System.String |> int
            | _ -> failwith "Invalid input"
    )
    |> Array.fold
        (fun (aAcc, bAcc) (a, b) -> (a :: aAcc), (b :: bAcc)

        )
        ([], [])

lists
|> Tuple.map List.sort
|> (fun (a, b) -> List.map2 (fun valA valB -> abs (valA - valB)) a b)
|> List.sum
|> (fun v -> printfn "answer 1: %i" v)

lists
|> (fun (listA, listB) ->
    let frequencies = listB |> List.countBy id |> Map

    listA
    |> List.map (fun v -> v * (frequencies |> Map.tryFind v |> Option.defaultValue 0)))
|> List.sum
|> (fun v -> printfn "answer 2: %i" v)
