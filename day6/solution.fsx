type Direction =
    | North
    | East
    | South
    | West

type GuardState =
    { Position: int * int
      Direction: Direction }

let parse (data: string array) =
    let map =
        data
        |> Array.map (
            Seq.map (function
                | '#' -> true
                | _ -> false)
            >> Array.ofSeq
        )

    let guardState =
        let position =
            let row = data |> Array.findIndex (Seq.contains '^')
            let col = data[row] |> Seq.findIndex ((=) '^')
            row, col

        { Position = position
          Direction = North }

    (map, guardState)

module Direction =
    let rotate =
        function
        | North -> East
        | East -> South
        | South -> West
        | West -> North

module Map =
    let tryIsBlocked (map: bool array array) (x, y) =
        try
            Some(map[x][y])
        with :? System.IndexOutOfRangeException ->
            None

module GuardState =
    let tryNext map state =
        let newPos =
            let currentPos = state.Position

            match state.Direction with
            | North -> (fst currentPos - 1), (snd currentPos)
            | East -> (fst currentPos), (snd currentPos + 1)
            | South -> (fst currentPos + 1), (snd currentPos)
            | West -> (fst currentPos), (snd currentPos - 1)

        Map.tryIsBlocked map newPos
        |> Option.map (fun nextPosIsBlocked ->
            if nextPosIsBlocked then
                { state with
                    Direction = Direction.rotate state.Direction }
            else
                { state with Position = newPos })


let solveDay1 map guardState =
    let positionsVisited =
        Seq.unfold (GuardState.tryNext map >> Option.map(fun s -> (s.Position,s))) guardState
    
    printfn $"Positions visited: %A{positionsVisited |> List.ofSeq}"
    
    let noOfPositionsVisited =
        positionsVisited
        |> Set.ofSeq
        |> Set.count
    
    printfn $"The guard visited {noOfPositionsVisited} locations"
    
let map, state=
    "day6/input.txt"
    |> System.IO.File.ReadAllLines
    |> parse

solveDay1 map state