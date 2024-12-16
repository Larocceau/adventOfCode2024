let neighbours (x, y) =
    [
        x, y + 1
        x, y - 1
        x + 1, y
        x - 1, y
    ]


let get (map: _ array array) coord =
    map[snd coord][fst coord]


let tryGet map coord =
    try
        Some (get map coord)
    with | :? System.IndexOutOfRangeException  -> None



let next coord map =
    let v = get map coord

    neighbours coord
    |> List.filter (
        tryGet map
        >> function
        | Some value when value = v + 1 -> true
        | _ -> false
    )

type Location = 
    {
        Value: int
        Next: (int* int) list
    }

let toLocations map =
    map
    |> Array.mapi (fun y row ->
        row |>
        Array.mapi(fun x value ->
        ((x, y), {
            Value = value
            Next = next (x,y) map 
        })
        )
    )  |> Array.concat    

let rec leadsToNine (locations: Map<_,_>) coord =
    match locations[coord] with
    | {Value = 9 } -> [coord ]
    | {Next = []} -> []
    | {Next = nextLocations } -> 
        nextLocations |> List.map (leadsToNine locations) |> List.concat

let rec routesToNine (locations: Map<_,_>) coord =
    match locations[coord] with
    | {Value = 9 } -> 1
    | {Next = []} -> 0
    | {Next = nextLocations } -> 
        nextLocations |> List.sumBy (routesToNine locations) 


let data =
    "day10/input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map(fun line -> line |> Seq.map (fun char -> $"{char}" |> int)|> Array.ofSeq)
    |> toLocations
    |> Map.ofArray

let zeros =
    data 
    |> Map.filter (fun _ v -> v.Value = 0 ) 
    |> Map.keys
let part1Solution =
    zeros
    |> Seq.sumBy (leadsToNine data >> List.distinct >> List.length)
    

printfn $"Solution part 1 %A{part1Solution}"

let part2Solution =
    zeros
    |> Seq.sumBy(leadsToNine data>>List.sumBy(routesToNine data))

printfn $"Solution part 2 %A{part1Solution}"
