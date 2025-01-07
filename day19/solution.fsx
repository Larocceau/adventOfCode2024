
type Color =
    | White
    | Blue
    | Black
    | Red
    | Green

let colorFromChar =
    function
    | 'w' -> White
    | 'u' -> Blue
    | 'b' -> Black
    | 'r' -> Red
    | 'g' -> Green
    | c -> failwith $"unknown color {c}"

type input = {
    Towels: Color array array
    Sequences: Color array list
}
    
let parseLine state (input: System.String) =
    input.Split ", "
    |> function
    | [| "" |] -> state
    | [| v |] -> {
            state with
                Sequences = (v |> Seq.map colorFromChar |> Array.ofSeq) :: state.Sequences
        }
    | towels -> {
            state with
                Towels = towels |> Array.map(fun towel -> towel |> Seq.map colorFromChar |> Array.ofSeq)
        }
    
    
module Array =
    
    let tryTakeHead sequence subSequence =
        let subSize = Array.length subSequence
        if sequence |> Array.length < subSize then
            None
        elif (Array.take subSize sequence) = subSequence then
            Some (Array.removeManyAt 0 subSize sequence)
        else
            None
            
    
[<TailCall>]
let rec possible  towels sequence =
    match towels with
    | [||] -> true
    | towels ->
        towels
        |> Array.exists(
         Array.tryTakeHead sequence
         >> Option.map (possible towels)
         >> _.IsSome
        )
        
let data =
    "day19\input.txt"
    |> System.IO.File.ReadLines
    |> Seq.fold parseLine {Towels = [||]; Sequences = []}
    

data.Sequences |> List.filter (possible data.Towels) |> List.length