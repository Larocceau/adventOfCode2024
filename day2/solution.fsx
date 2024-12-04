let fileName = "day2/input.txt"

let allPosOrNeg v =
    [ (Array.forall ((<>) 0))
      (Array.partition ((>=) 0)
       >> function
           | [||], _
           | _, [||] -> true
           | _ -> false) ]
    |> List.forall (fun f -> f v)

let data =
    fileName
    |> System.IO.File.ReadAllLines
    |> Array.map (fun x -> x.Split " " |> Array.map int)

let isSafe =
    Array.pairwise
    >> Array.map (fun (a, b) -> b - a)
    >> (fun v ->
        [ allPosOrNeg; Array.forall (abs >> (fun x -> x > 0 && x < 4)) ]
        |> List.forall (fun f -> f v))

let versions (report: int array) =
    [ report
      for i in [ 0 .. (report.Length - 1) ] do
          Array.removeAt i report ]


for row in data do
    printfn $"Data:%A{List.ofSeq row} is safe:{isSafe row}"

let numberSafe = data |> Array.filter isSafe |> Array.length

printfn $"There are {numberSafe} safe reports"

let safeWithDampener =
    data |> Array.filter (versions >> List.exists isSafe) |> Array.length

printfn $"There are {safeWithDampener} safe reports after dampening"
