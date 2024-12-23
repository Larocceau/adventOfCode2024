open System
open System.Text.RegularExpressions

let tee  f x =
    f x
    x

type Robot = {
    Velocity: int * int
    Position: int * int
}

let rollOver v maxV =
    if v < 0 then
        maxV + v
    elif v >= maxV then
        v - maxV
    else
        v

module Robot =
    
    let handleTick maxX maxY ({Position = (px,py); Velocity = (vx, vy)} as robot) =
        let newPos= rollOver (px + vx) maxX, rollOver (py + vy) maxY
        
        {
            robot with Position = newPos
        }
        

open Robot

let parseLine line =
   let reMatch = Regex.Match(line, "(\d+),(\d+) v=(\-?\d+),(\-?\d+)").Groups |> Seq.tail |> Seq.map (_.Value >> int) |> List.ofSeq
   
   {
       Position = reMatch[0], reMatch[1]
       Velocity = reMatch[2], reMatch[3]
   }

let applyN n f =
    Seq.initInfinite (fun _ -> f)
    |> Seq.take n
    |> Seq.reduce (>>)
    
type Quadrant =
    | Lt
    | Rt
    | Lb
    | Rb 
    
let quadrant maxX maxY position=
    match (fst position) - (maxX / 2 ) |> Math.Sign, (snd position) - (maxY /2) |> Math.Sign with
    | -1, -1 -> Some Lt
    | 1, -1 -> Some Rt
    | -1, 1 -> Some Lb
    | 1, 1 -> Some Rb
    | _ -> None

let visualise maxX maxY robots =
    let positionCount = robots |> Array.countBy _.Position |> Map.ofArray
    [| 0..maxY-1|]
    |> Array.map(fun y ->
            [0..maxX-1]
            |> List.map(fun x ->
                Map.tryFind (x, y) positionCount  |> Option.map string |> Option.defaultValue "."
                )
            |> String.concat ""
            
        )
   
            
let print maxX maxY robots =
    visualise maxX maxY robots
    |> Array.iter (fun str -> printfn $"{str}")
    
    printfn ""
    

let findSymmetrical=
    let rec implementation index width height data = 
        let half = (width / 2)
        let newState = Array.map (handleTick width height) data
        
        let left, right =
            newState
            |> Array.map _.Position
            |> Array.filter (fun (x,_ ) -> x <> half)
            |> Array.partition (fun (x, _) -> x < half)
        
        let leftRigtified =
            left
            |> Array.map(
                fun (x, y) ->
                    let x = half + (half - x)
                    (x, y)
                )
            |> Array.sort
        
        if leftRigtified = (right |> Array.sort) then
            newState
        else
            implementation (index + 1) width height newState 
    implementation 0
    
let file, width, height =
    System.IO.File.ReadAllLines "day14/input.txt", 101, 103

let data =
    file
    |> Array.map(parseLine)
    |> tee (print width height)

let part1 =
    data
    |> Array.map(applyN 100 (handleTick width height))
    |> tee (print width height)
    |> Array.countBy(_.Position >> quadrant width height)
    |> Array.choose (fun (k, v) -> k |> Option.map(fun _ -> v))
    |> Array.reduce (*)
    


let sym = findSymmetrical width height data