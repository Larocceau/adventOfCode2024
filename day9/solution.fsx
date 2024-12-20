open System.IO
open System.Numerics

let data =
    "2333133121414131402"
    File.ReadAllLines "day9/input.txt"
    |> (fun l -> l[0])
    |> Seq.map (fun v -> v.ToString() |> int)
    |> List.ofSeq


let checkSum =
    List.mapi (fun i v -> v |> Option.map ((*) i >> BigInteger))
    >> List.choose id
    >> List.sum

module Part1 =
    let expand =
        List.mapi (fun index v ->
            let id = if index % 2 = 0 then Some(index / 2) else None
            [ 1..v ] |> List.map (fun _ -> id))
        >> List.concat

    let compress (data: int option list) =

        let rec implementation p1 p2 (data: int option list) =
            

            if p1 = p2 then
                data
            else
                match data[p1], data[p2] with
                | _, None -> implementation p1 (p2 - 1) data
                | Some _, _ -> implementation (p1 + 1) p2 data
                | None, Some v ->
                    let afterSwap = data |> List.updateAt p1 (Some v) |> List.updateAt p2 None
                    implementation (p1 + 1) (p2 - 1) afterSwap

        implementation 0 (data.Length - 1) data



    let solve = expand >> compress >> checkSum

module Part2 =

    type File = { Id: int; Size: int }

    type Entry =
        | File of File
        | Gap of int

    let gapsAndFiles data =

        let files, gaps =
            data
            |> List.mapi (fun i v ->
                match i % 2 with
                | 1 -> Gap v
                | _ -> File { Id = (i / 2); Size = v })
            |> List.partition _.IsFile
        (files |> List.map (fun (File f) -> f), gaps |> List.map (fun (Gap g) -> g) |> (fun x ->List.append x [0] ) )
        
    let expand (files: File list, gaps) =

        (files, gaps)
        ||> List.zip
        |> List.map (fun (file, gap) ->
            [ for _ in [ 0 .. (file.Size - 1) ] do
                  Some file.Id
              for _ in [ 0 .. (gap - 1) ] do
                  None ])
        |> List.concat
    
    
    let print =
        expand
        >> List.map (
            function
            | None -> "..."
            | Some v -> v |> string 
            )
        >> string
    let sort (files: File list, gaps) =
        
        let rec implementation fileId (files: File list, gaps) =
            if fileId % 5 = 0 then
                let _ = System.Console.ReadLine()
                ()
            print (files, gaps)
            if fileId % 100 = 0 then printfn "Handled 100 files"
            if fileId = 0 then
                files, gaps
            else
                let pointer = files |> List.findIndex (fun f -> f.Id = fileId)
                let file = files[pointer]
                                
                let gapIndex =
                    gaps
                    |> List.indexed
                    |> List.tryFind (
                        fun (index, gap) ->
                            (index < pointer && gap >= file.Size)
                        )
                    |> Option.map fst
                
                let newState =
                    match gapIndex with
                    | None -> (files, gaps)
                    | Some gapIndex ->
                                                
                        // Move the file to the correct gap
                        files
                        |> List.removeAt pointer
                        |> List.insertAt (gapIndex + 1) file,
                        gaps
                        // combine the gaps we found

                        |> List.removeAt pointer
                        |> List.updateAt (pointer - 1) (gaps[pointer - 1 ] + gaps[pointer] + file.Size )
                        |> List.updateAt gapIndex (gaps[gapIndex] -  file.Size)
                        |> List.insertAt gapIndex 0
                    
                implementation (fileId - 1) newState
                    
        
        implementation (files.Length - 1) (files, gaps) 
                
                
                
            

    let solve =
        gapsAndFiles
        >> sort
        >> expand
        >> checkSum




// let part1CheckSum = Part1.solve data
// printfn $"the checksum is {part1CheckSum}"

let part2CheckSum = Part2.solve data
printfn $"the checksum for part 2 is {part2CheckSum}"
// 9902485241939 is too high
// 6398088927248 is incorrect