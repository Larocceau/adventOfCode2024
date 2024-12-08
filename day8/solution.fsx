open System

let parse: string seq -> ((int * int) list) list =
    Seq.mapi (fun y row ->
        row
        |> Seq.mapi (fun x v ->
            match v with
            | '.' -> None
            | c -> Some(c, (x, y))

        ))
    >> Seq.concat
    >> Seq.choose id
    >> Seq.groupBy fst
    >> Seq.map (fun (_, v) -> v |> Seq.map snd |> List.ofSeq)
    >> List.ofSeq

let linearFunction (p1: int * int) p2 =
    let (x1, y1), (x2, y2) = if fst p1 < fst p2 then (p1, p2) else (p2, p1)

    let dx = x2 - x1
    let dy = y2 - y1

    let slope = decimal (dy) / decimal (dx)
    let intercept = -(slope * decimal x1 - decimal y1)
    printfn $"Slope is {slope}"
    fun x -> slope * x + intercept


let calcAntiNodes (x1, y1) (x2, y2) =
    let xDistance = abs (x1 - x2)
    let yDistance = abs (y1 - y2)

    [ (x1 + (if x1 > x2 then xDistance else -xDistance), y1 + (if y1 > y2 then yDistance else -yDistance))
      (x2 + (if x2 > x1 then xDistance else -xDistance), y2 + (if y2 > y1 then yDistance else -yDistance)) ]

let calcAntiNodesBetter mapWidth (mapHeight: int) p1 p2 =
    let f = linearFunction p1 p2

    [ 0 .. (mapWidth - 1) ]
    |> List.choose (fun x ->
        let y = Decimal.Round(f x, 5)

        if (y % 1m = 0) && y >= 0 && (int) y < mapHeight then
            printfn "put in!"
            Some(x, int y)
        else
            None)



let data = System.IO.File.ReadAllLines "day8/input.txt"

let gridWidth, gridHeight = data[0].Length, data.Length

let calcInMap (x, y) =
    x >= 0 && x < gridWidth && y >= 0 && y < gridHeight

let antennas = data |> parse

let antiNodes =
    antennas
    |> List.map (fun l ->
        List.allPairs l l
        |> List.filter (fun (a, b) -> not (a = b))
        |> List.map (fun (a1, a2) -> calcAntiNodes a1 a2)
        |> List.concat)
    |> List.concat
    |> List.distinct
    |> List.filter calcInMap


printfn $"there are {antiNodes.Length} antinodes"

let betterAntiNodes =
    let calcAntiNodes = calcAntiNodesBetter gridWidth gridWidth

    antennas
    |> List.map (fun l ->
        List.allPairs l l
        |> List.filter (fun (a, b) -> not (a = b))
        |> List.map (fun (a, b) -> calcAntiNodes a b)
        |> List.concat)
    |> List.concat
    |> List.distinct

printfn $"there are {betterAntiNodes.Length} antinodes"

data
|> Array.mapi (fun y data ->
    data
    |> Seq.mapi (
        (fun x v ->
            if betterAntiNodes |> List.contains (x, y) && (v = '.') then
                '@'
            else
                v)
    )
    |> Array.ofSeq
    |> System.String)
|> fun lines -> System.IO.File.WriteAllLines("day8/output.txt", lines)
