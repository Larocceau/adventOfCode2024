
let neighbours (x, y) =
    [
        x, y + 1
        x, y - 1
        x + 1, y
        x - 1, y
    ]
    


let data =
    System.IO.File.ReadAllLines "day16/sampleInput.txt"
    |> Seq.mapi(fun y row -> row |> Seq.mapi (fun x v -> (x,y), v))
    |> Seq.concat

let start =
    data
    |> Seq.find (snd >>(=) 'S')
    |> fst

let finish =
    data
    |> Seq.find (snd >>(=) 'E')
    |> fst



let positions =
    data
    |> Seq.choose ( fun (pos, value) ->
        match value with
        | '#' -> None
        | _ -> Some pos
        )

let network =
    positions
    |> Seq.map(fun pos ->
        let existingNeigbours =
            pos
            |> neighbours
            |> List.filter(fun pos -> Seq.contains pos positions)
        pos, existingNeigbours
        )
    |> Map.ofSeq
    
[<TailCall>]
let rec routesToDest (network: Map<int*int,(int*int) list>) routeSoFar dest pos =
    
    if pos = dest then
        [pos::routeSoFar]
    else
        network[pos]
        |> List.filter (fun pos -> routeSoFar |> List.contains pos |> not)
        |> List.collect (routesToDest network (pos :: routeSoFar) dest)

type Direction =
    | North
    | East
    | South
    | West

module Direction =
    let rotate =
        function
        | North -> West
        | East -> North
        | South -> East
        | West -> South
    
module Position =
    
    let next direction (x, y) =
        match direction with
        | North -> x, y - 1
        | East -> x + 1, y
        | South -> x, y + 1
        | West -> x - 1, y
        
type Instruction =
    | Straight
    | Turn
    
let instructions direction route =
    
    let rec implementation direction acc route =
    
        match route with
        | []
        | [ _ ] -> acc
        | pos :: oneToLast :: rest ->
            if (Position.next direction pos) = oneToLast then
                implementation direction (Straight :: acc) (oneToLast :: rest)
            else
                let newAcc =
                    if acc |> List.tryHead |> Option.contains Turn then
                        acc
                    else
                        Turn :: acc
                implementation (Direction.rotate direction) newAcc route
    
    implementation direction [] route |> List.rev

let print route =
    let maxX =  route |> List.map fst |> List.max 
    let maxY = route |> List.map snd |> List.max
    
    [0..maxY]
    |> List.iter(
        fun y -> 
                let line =
                    [|0..maxX|]
                    |> Array.map(fun x ->
                            if List.contains (x, y) route then
                                'x'
                            else
                                '.'
                        )
                    |> System.String
               
                printfn $"{line}"
            )

let cost instructions =
    let turns, straights = List.partition ((=) Turn) instructions
    
    turns.Length * 1000 + straights.Length

let routes =
    routesToDest network [] finish start
    |> List.map List.rev

printfn $"there are {routes.Length} routes"

let cheapest =
    routes
    |> List.minBy (instructions East >> cost)

print cheapest

let cheapestInstructions = instructions East cheapest
let cheapestCost = cost cheapestInstructions

    
printfn $"the cheapest route has the following instructions : %A{cheapestInstructions} and costs {cheapestCost}"