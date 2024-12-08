open System.Numerics

let parseLine (v: string) =
    let total, valuesWord =
        match v.Split(": ") with
        | [| w1; w2 |] -> (w1, w2)
        | _ -> failwith "Invalid input"

    let values = valuesWord.Split ' ' |> Array.map BigInteger.Parse |> List.ofArray

    (BigInteger.Parse total, values)

let parseFile = Array.map (parseLine)

[<TailCall>]
let permutations<'a> (values: 'a list) (length: _) =

    let rec implementation (acc: 'a list list) =
        match acc with
        | v when v[0].Length = length -> v
        | v ->
            v
            |> List.map (fun sequence -> values |> List.map (fun value -> value :: sequence))
            |> List.collect id
            |> implementation

    implementation [ [] ]


let solve (input: (_ * _ list) list) operations =

    let longestSequence = input |> List.map (snd >> _.Length) |> List.max

    let operatorSequencesByLength =
        [ 1 .. (longestSequence - 1) ]
        |> List.map (fun i -> (i, permutations operations i))
        |> Map.ofList

    let total =
        input
        |> List.choose (fun (target, parts) ->
            operatorSequencesByLength[parts.Length - 1]
            |> List.tryFind (fun opSequence ->

                let foldRes =
                    (parts[0], opSequence, parts |> List.tail)
                    |||> List.fold2 (fun acc operation value -> operation acc value)

                foldRes = target)
            |> Option.map (fun _ -> target))
        |> List.sum

    printfn $"The sum of all valid calibrations when is: {total}"

let puzzleInput =
    System.IO.File.ReadAllLines "day7/input.txt" |> parseFile |> List.ofArray

printfn "Solving part 1: * and +"
solve puzzleInput [ (*); (+) ]
printfn "sovling part 2: * and + and concat"
solve puzzleInput [ (*); (+); (fun a b -> BigInteger.Parse(string a + string b)) ]