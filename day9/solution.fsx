

open System.Drawing
open System.Numerics

let data =
    // "2333133121414131402"
    System.IO.File.ReadAllLines "day9/input.txt"
    |> (fun l -> l[0])
    |> Seq.map( fun v ->
        v.ToString() |> int) |> List.ofSeq

module Part1 = 
    let expand =
        List.mapi(
            fun index v ->
                let id =
                    if index % 2 = 0 then
                        Some (index / 2)
                    else
                        None
                [1..v] |> List.map (fun _ -> id)
            )
        >> List.concat

    let compress (data: int option list) =
        
        let rec implementation p1 p2 (data: int option list) =
            if p1 % 100 = 0 then
                printfn "processed 100 entries"
            if p1 = p2 then
                data
            else
                match data[p1], data[p2] with
                | _ , None -> implementation p1 (p2 - 1) data
                | Some _, _ -> implementation (p1 + 1) p2 data
                | None, Some v ->
                    let afterSwap =
                        data |> List.updateAt p1 (Some v) |> List.updateAt p2 None
                    implementation (p1 + 1) (p2 - 1) afterSwap
        
        implementation 0 (data.Length - 1)  data

    let checkSum =
        List.choose id
        >> List.mapi (fun i v-> BigInteger(i * v))
        >> List.sum


    let solve =
        expand
        >> compress
        >> checkSum    

let day1CheckSum = Part1.solve data
printfn $"the checksum is {day1CheckSum}"