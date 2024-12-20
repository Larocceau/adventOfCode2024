open System.Diagnostics

type Direction =
    | North
    | East
    | South
    | West

type Guard =
    { Position: int * int
      Direction: Direction }

type Map = bool array array

let parse (data: string array) : (Map * Guard) =
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
        if x >= 0 && x < map.Length && y >= 0 && y < map[0].Length then
            Some(map[x][y])
        else
            None

module Guard =
    let tryMove map guard =
        let newPos =
            let x, y = guard.Position

            match guard.Direction with
            | North -> (x - 1), y
            | East -> x, (y + 1)
            | South -> (x + 1), y
            | West -> x, (y - 1)

        Map.tryIsBlocked map newPos
        |> Option.map (fun nextPosIsBlocked ->
            if nextPosIsBlocked then
                { guard with
                    Direction = Direction.rotate guard.Direction }
            else
                { guard with Position = newPos })

    let loops map =

        let rec implementation previousStates state =
            match tryMove map state with
            | None -> false
            | Some nextState ->
                if (previousStates |> List.contains nextState) then
                    true
                else
                    implementation (state :: previousStates) nextState

        implementation []

let solveDay1 map guardState =
    let positions =
        List.unfold (Guard.tryMove map >> Option.map (fun s -> (s.Position, s))) guardState


    printfn $"Positions visited: %A{positions}"

    let uniquePositions = positions |> List.distinct

    printfn $"The guard visited {uniquePositions |> List.length} unique locations"
    
    let sw  = Stopwatch()
    sw.Start()

    let mapsWithLoops =
        uniquePositions
        |> List.map (fun (x, y) -> Array.updateAt x (Array.updateAt y true map[x]) map)
        |> List.filter (fun map -> Guard.loops map guardState)

    sw.Stop()
    printfn $"spent {sw.Elapsed.TotalSeconds} seconds on part 2"
    printfn $"There are {mapsWithLoops.Length} maps with loops"

let map, state = "day6/input.txt" |> System.IO.File.ReadAllLines |> parse

let sw = Stopwatch()
sw.Start()
solveDay1 map state
sw.Stop()
printfn $"spent {sw.Elapsed.TotalSeconds} seconds on part 1"