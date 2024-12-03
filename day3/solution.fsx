

let fileName = "day3/input.txt"

let multRegex=  @"mul\(([\d]+)\,([\d]+)\)"

let dontRegex = @"don't\(\)"

let doRegex = @"do\(\)"

module Regex =
    open System.Text.RegularExpressions
    
    let matches pattern input =
        Regex.Matches(input, pattern)

let text =
    fileName
    |> System.IO.File.ReadAllText

    
let mults =
    text
    |> Regex.matches multRegex
    |> Seq.map(fun x -> (x.Index, (int x.Groups[1].Value * int x.Groups[2].Value)))

    
let solution1 =
    mults
    |> Seq.map snd
    |> Seq.sum
    
printfn $"the first solution is: {solution1}"

let dos =
    text
    |> Regex.matches doRegex
    |> Seq.map(_.Index)
    
let donts =
    text
    |> Regex.matches dontRegex
    |> Seq.map(_.Index)

type Operations =
    | Do
    | Dont
    | Mult of int

let operations =
    [
        for index in dos do
            (index, Do)
        for index in donts do
            (index, Dont)
        for index, values in mults do
            (index, Mult values)
    ]
    |> List.sortBy fst
    |> List.map snd
        
    
operations
|> List.fold (
    fun (counting, total) instruction ->
        match instruction with
        | Do -> (true, total)
        | Dont -> (false, total)
        | Mult value ->
            (counting,
                if counting then
                    total + value
                else
                    total
             )
        
        )(true, 0)