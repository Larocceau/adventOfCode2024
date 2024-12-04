open System
open System.Text.RegularExpressions

let cols<'a> (rows: 'a array array) =
    rows[0]
    |> Array.mapi
           ( fun i _ -> rows |> Array.map (fun row -> row[i]) )

let diags (rows: 'a array array) =
    let maxY = rows[0].Length - 1
    let maxX = rows.Length - 1
    let maxIndex = maxX + maxY
    let inGrid x = x >= 0 && x <= maxX
        
    
    [|
        for i in [(-maxY)..maxIndex] do
            let res = [|
                for y in [0..maxY] do
                        let x =  i-y 
                        if inGrid x then
                            (rows[x][y], rows[ maxX - x][y])
                |]
            (res |> Array.map fst,
            res |> Array.map snd)
    |] |> fun x -> (x|> Array.map fst, x |> Array.map snd)
                    
              
let input =
    "day4/input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map( _.ToCharArray())

let lines input  =
    let diag1, diag2 = diags input
    let originals = Array.concat [|
        input
        cols input
        diag1
        diag2
        |]
    let reversed = originals |> Array.map (Array.rev)
    
    Array.concat [| originals; reversed |]
    
    
let myDiags =
    input
    |> lines
    |> Array.map (
        String
        >> fun x -> Regex.Count(x,"XMAS"))
    |> Array.sum
    

let isMas (posX,posY) (grid: char array array) =
    match grid[posX][posY] with
    | 'A' ->
        let rows = [
            [
                grid[posX-1][posY-1]
                grid[posX+1][posY+1]
            ]
            [
                grid[posX-1][posY+1]
                grid[posX+1][posY-1]
            ]
        ]
        printfn $"rows: {rows}"
        let res = rows |> List.forall (
                             List.sort >>
                             (function
                                | ['M';'S'] -> true | _ -> false))
        printfn $"is x: {res}"
        res
        
    | _ -> false
    

input
|> cols
|> (fun cols -> 
    [
        for x in [1..(cols.Length-2)] do
            for y in [1..(cols[1].Length-2)] do
                isMas(x,y) cols
    ]
     ) |> List.countBy id