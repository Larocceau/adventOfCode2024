open System
open System.Diagnostics
open System.Text.RegularExpressions

type Robot = {
    Velocity: int * int
    Position: int * int
}

let rollOver v maxV =
    if v < 0 then
        maxV + v
    elif v > maxV then
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

let data =
    System.IO.File.ReadAllLines "day14/sampleInput.txt"
    |> Array.map(parseLine)
    |> Array.map(applyN 100 (handleTick 11 7))
    |> Array.countBy(_.Position >> quadrant 11 7)
    |> Array.choose (fun (k, v) -> k |> Option.map(fun _ -> v))
    |> Array.reduce (*)
    
    
    
    
let sw = Stopwatch()

sw.Start()
let (myFun:string -> string) = applyN 5000 id