open System.IO
open System.Numerics

let data =
   "2333133121414131402"
    // File.ReadAllLines "day9/input.txt"
    // |> (fun l -> l[0])
    |> Seq.map( fun v ->
        v.ToString() |> int) |> List.ofSeq
    

let checkSum =
    List.mapi (fun i v-> v |> Option.map((*) i >> BigInteger))
    >>List.choose id
    >> List.sum

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



    let solve =
        expand
        >> compress
        >> checkSum
        
module Part2 =
    
    type File = {
        Id: int
        Length: int
    }
    type Element =
        | Gap of length:int
        | File of File 
        
    let categorize =
        List.mapi(
        fun index length ->
            if index % 2 = 0 then
                File {
                    Length = length
                    Id =  index / 2
                }
            else
                Gap length
        )
    let gapsAndFiles data =
        let gaps, files =
            data
            |> categorize
            |> List.partition _.IsGap
            
        (
            gaps |> List.map(function | Gap v -> v),
            files |> List.map(function | File f -> f)
        )
        
    let sort gaps files =
        
        let rec  implementation (gaps: int list) (files: File list) pointer  =
            if pointer % 100 = 0 then
                printfn "processed 100 entries"
            
            if pointer = 0 then
                gaps, files
            else
                let file = files[pointer]
                let gap = gaps |> List.tryFindIndex (fun g -> g >= file.Length) |> Option.defaultValue(pointer)               
                let gaps, files = 
                    // match gap with
                    // | Some gap ->
                        let newGapSize = gaps[gap] - file.Length
                        let newGaps =
                            gaps
                            |> List.removeAt pointer
                            |> List.updateAt (pointer - 1) (gaps[pointer - 1] + gaps[pointer] + file.Length)
                            |> List.updateAt gap newGapSize
                            |> List.insertAt gap 0
                        let newFiles = files |> List.removeAt pointer |> List.insertAt (gap + 1) file 
                        
                        newGaps, newFiles
                    // | None ->
                    //     gaps, files
                    
                implementation gaps files (pointer - 1)
                
        implementation (List.append gaps [0]) files (files.Length - 1 )
            
    let solve data =
       let gaps, files = gapsAndFiles data
       
       let sortedGaps, sortedFiles = sort gaps files
       printfn $"Sorted files: %A{sortedFiles |> List.map _.Id}"
       (sortedGaps, sortedFiles)
       ||> List.map2 ( fun gap file ->
            [
                for _ in [0..file.Length-1] do
                    Some file.Id
                for _ in [0..gap-1] do
                    None
            ])
        |> List.concat
        |> checkSum
        

        

// let part1CheckSum = Part1.solve data
// printfn $"the checksum is {part1CheckSum}"

let part2CheckSum = Part2.solve data
printfn $"the checksum for part 2 is {part2CheckSum}"
// 9902485241939 is too high