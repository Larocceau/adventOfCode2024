open System.Diagnostics

type OpType =
    | ADV
    | BXL
    | BST
    | JNZ
    | BXC
    | OUT
    | BDV
    | CDV

let OpTypeFromOpCode =
    function
    | 0 -> ADV
    | 1 -> BXL
    | 2 -> BST
    | 3 -> JNZ
    | 4 -> BXC
    | 5 -> OUT
    | 6 -> BDV
    | 7 -> CDV
    | _ -> failwith "invalidOpCode"


type Computer =
    { AReg: int
      BReg: int
      CReg: int
      Instructions: int array
      Pointer: int }

let comboOperant computer =
    function
    | 0 -> 0
    | 1 -> 1
    | 2 -> 2
    | 3 -> 3
    | 4 -> computer.AReg
    | 5 -> computer.BReg
    | 6 -> computer.CReg
    | _ -> failwith "invalid combo operant"


let handle computer =
    computer.Instructions
    |> Array.tryItem computer.Pointer
    |> Option.map (fun opcode ->
        let pointer = computer.Pointer
        let operant = computer.Instructions |> Array.item (pointer + 1)

        match OpTypeFromOpCode opcode with
        | ADV ->
            let comboOperant = comboOperant computer operant
            let denominator = pown 2 comboOperant 
            let res = computer.AReg / denominator 

            (None,
             { computer with
                 AReg = res
                 Pointer = pointer + 2 })
        | BXL ->
            let res = computer.BReg ^^^ operant

            (None,
             { computer with
                 BReg = res
                 Pointer = pointer + 2 })
        | BST ->
            let res = (comboOperant computer operant) % 8

            (None,
             { computer with
                 BReg = res
                 Pointer = pointer + 2 })
        | BXC ->
            let res = computer.BReg ^^^ computer.CReg
            (None, {
                computer with
                    BReg = res
                    Pointer = pointer + 2
            })
        | JNZ ->
            let pointer =
                match computer.AReg with
                | 0 -> pointer + 2
                | _ -> operant

            (None, { computer with Pointer = pointer })
        | OUT ->
            let output = (comboOperant computer operant) % 8
            (Some output, { computer with Pointer = pointer + 2 })
        | BDV ->
            let res = computer.AReg / pown 2 (comboOperant computer operant) 

            (None,
             { computer with
                 BReg = res
                 Pointer = pointer + 2 })
        | CDV ->
            let res = computer.AReg / pown 2 (comboOperant computer operant)

            (None,
             { computer with
                 CReg = res
                 Pointer = pointer + 2 }))
   



//let computer = computer |> handle |> _.Value |> snd


let rec outputsNext computer n =
    
    match handle computer with
    | None -> None
    | Some (output, computer) ->
        match output with
        | None -> outputsNext computer n
        | Some v when v = n -> Some computer
        | Some _ -> None
        
let outputMatches (expected: int array) computer =
    let mutable computer = computer
    expected
    |> Array.forall( fun n -> 
        match outputsNext computer n with
        | Some c ->
            computer <- c
            true
        | _ -> false)
    
//let list = [| 2;4;1;3;7;5;0;3;1;4;4;7;5;5;3;0 |]
let list = [|0;3;5;4;3;0|]
let computer =
    { AReg = 2025
      BReg = 0
      CReg = 0
      Pointer = 0
      Instructions = list}
    

let sw = Stopwatch()

sw.Start()
try
    Seq.initInfinite (fun i -> (if i % 10000000 = 0 then printfn $"running {i}"); {computer with AReg =i} )
    |> Seq.find(outputMatches list)
finally
    printfn $"That took {sw.Elapsed} milliseconds"