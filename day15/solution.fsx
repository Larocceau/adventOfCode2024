

open System

let tee f x =
    f x
    x
type Direction =
    | Up
    | Down
    | Left
    | Right    

type Game = {
    Height: int
    Width: int
    Boxes: (int * int) array
    Walls: (int * int) array
    Robot: int * int
}

let print game =
    [0..game.Height]
    |> List.iter(
        fun y ->
            let stringVal =
                [|0..game.Width|]
                |> Array.map(
                    fun x ->
                        if game.Robot = (x,y) then
                            '@'
                        else if Array.contains (x, y) game.Boxes then
                            '0'
                        else if Array.contains (x,y) game.Walls then
                            '#'
                        else
                            '.'
                    ) |> String
            printfn $"{stringVal}"
                )

let parseline (game: Game, moves) (line: string) =
    match line |> Seq.tryHead with
    | Some '#' ->
        let y = game.Height

        let walls, boxes =
            line
            |> Seq.mapi ( fun x char ->
                match char with
                | '#' -> Some (true, (x,y))
                | 'O' -> Some (false, (x,y))
                | _ -> None
            )
            |> Seq.choose id
            |> Array.ofSeq
            |> Array.partition fst
        
        let robot =
            line
            |> Seq.tryFindIndex ((=) '@')
            |> Option.map(fun x -> x, y)
            |> Option.defaultValue game.Robot
        ({
            game with
                Walls =  Array.concat [ game.Walls; walls |> Array.map snd ]
                Boxes = Array.concat [ game.Boxes; boxes |> Array.map snd ]
                Robot = robot
                Height = game.Height + 1
                Width = max game.Width line.Length
        }, moves)
    | _ ->
        let newMoves =
            line
            |> Seq.choose(
                function
                | '>' -> Some Right
                | '<' -> Some Left
                | '^' -> Some Up
                | 'v' -> Some Down
                | _ -> None)
            |> List.ofSeq
            
        (game, moves @ newMoves)
        

let next direction (x, y)=
    match direction with
    | Up -> x, y - 1
    | Down -> x, y + 1
    | Left -> x - 1, y
    | Right -> x + 1, y

let moveBoxTo walls boxes position direction =
    
    let rec CheckNext position = 
        let checking = next direction position
        match walls |> Array.contains checking with
        | true -> None
        | false ->
            match boxes |> Array.contains checking with
            | false -> Some checking
            | true ->
                CheckNext checking 
                
    CheckNext position
    
let handleMove game direction =
    (match moveBoxTo game.Walls game.Boxes game.Robot direction with
    | None -> game
    | Some movingTo ->
        let robotPos = next direction game.Robot
        
        {
            game with
                Robot = robotPos
                Boxes =
                    game.Boxes
                    |> Array.append [| movingTo |]
                    |> Array.filter ((<>) (next direction game.Robot ))
        } 
        
    )
    
let game, moves =
    let lines = System.IO.File.ReadAllLines "day15/input.txt"
    (({Boxes = Array.empty ;Walls = Array.empty ; Robot = 0,0;Height = 0; Width = 0}, List.empty), lines)
    ||> Array.fold parseline

let finalState =
    (game, moves)
    ||> List.fold handleMove

let part1Solution =
    finalState
    |> _.Boxes
    |> Array.map(fun (x, y) -> 100 * y + x)
    |> Array.sum