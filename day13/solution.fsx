
open System.Net.Mail
open System.Text.RegularExpressions


let machineRegex =
    Regex ("Button A: X\+(\d+), Y\+(\d+)\s+Button B: X\+(\d+), Y\+(\d+)\s+Prize: X=(\d+), Y=(\d+)", RegexOptions.Singleline)
    
type Machine = {
    btnA: int * int
    btnB: int * int
    PrizeLocation: int * int
    Number: int
    }

let handlePress deltaGetter ({PrizeLocation = (xPos, yPos)} as machine) =
    
    let deltaX, deltaY = deltaGetter machine
    {
        machine with
            PrizeLocation = (xPos - deltaX, yPos - deltaY)
    }


let divisions tokens =
    let maxA = tokens / 3
    
    [
        for i in [0..maxA] do
            i, tokens - i * 3
    ]

let attempts tokens =
    divisions tokens
    |> List.map (fun (aCount, bCount) ->
        [
            yield! List.init aCount (fun _ -> handlePress _.btnA)
            yield! List.init bCount (fun _ -> handlePress _.btnB)
        ] |> List.reduce (>>)
        )
    

let smallestAttempt n (machine: Machine) =
    printfn $"dealing with machine {machine.Number}"
    [1..n]
    |> List.tryFind (
        attempts
        >> List.exists (fun attempt ->
            let (finalState: Machine) = attempt machine
            finalState.PrizeLocation = (0,0)
             )
        )

let machineFromMatch index (v: Match): Machine =
    let values = v.Groups |> Seq.tail |> Seq.map (_.Value >> int) |> List.ofSeq
    printfn $"extracted values: %A{values}"
    {
        btnA = values[0], values[1]
        btnB = values[2], values[3]
        PrizeLocation = values[4], values[5]
        Number = index
    }


let parse =
    machineRegex.Matches
    >> Seq.mapi(machineFromMatch)


let data =
    System.IO.File.ReadAllText "day13/input.txt"
    |> parse
    |> List.ofSeq
  
let resPart1 =
    data
    |> List.choose(smallestAttempt 500 )
    |> List.sum

printfn $"The solution to part 1 is {resPart1}"

//21836 is too low