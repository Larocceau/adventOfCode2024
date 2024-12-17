
open System.Numerics
open System.Collections.Generic

let (|EvenNoOfDigits |_ |) (v: bigint)=
    let asString =
        v
        |> string
    if 

        asString|> (fun v -> v.Length % 2 = 0)
    then  Some asString
    else None

let (|Value|_|) comparisonValue (v: 'a) =
    v = comparisonValue


let memoize f =
    let store = Dictionary()

    (fun x ->
        match store.TryGetValue x with
        | true, v -> v
        | _ -> 
            let v = f x
            store.Add(x, v)
            v
    
    )
let applyN<'a> n (f: 'a -> 'a) : 'a -> 'a =
    Seq.init n (fun _ -> f)
    |> Seq.reduce (>>)

module Stone =
    let handleTick (v: bigint) =
        v
        |> function
        | Value (bigint(0)) -> [bigint 1]
        | EvenNoOfDigits v -> 
            v 
            |> Seq.splitInto 2
            |> Seq.map ( System.String >> BigInteger.Parse)
            |> List.ofSeq
        | v -> [v * bigint 2024]
            
    
    let handleTickMemo =
        memoize handleTick

    let rec handleTickN n v =
        match n with
        | 0 ->  v
        | n -> handleTickN (n - 1) (v |> List.map handleTick |> List.concat)

    let rec handleTickNManualMemo () n v =
        let store = Dictionary()

        
        match n with
        | 0 ->  v
        | n -> 
            let key = n,v
            match store.TryGetValue (key ) with
            | true, res -> res
            | _ -> 
                let res = handleTickN (n - 1) (v |> List.map handleTick |> List.concat)

                store.Add(key, res)
                res
    
    let handleTickNMemo =
        memoize handleTickN


module StoneList =
    let handleTick =
        List.map Stone.handleTickMemo
        >> List.concat

    let hanldeTickN n =
        let memoizedF = Stone.handleTickNManualMemo ()
        List.map (memoizedF n)
        >> List.concat



let data =
    "day11/input.txt"
    |> System.IO.File.ReadAllLines
    |> (fun x -> x[0])
    |> (fun x -> x.Split " ")
    |> Array.map(BigInteger.Parse)
    |> List.ofArray


let solutions n =  [
    (List.map List.singleton
    >> StoneList.hanldeTickN n)
    applyN n StoneList.handleTick
]

let sw = System.Diagnostics.Stopwatch ()

solutions 75
|> List.iteri( fun solverIndex solver ->
    printfn $"approach {solverIndex}"
    sw.Restart()
    let solution = solver data
    sw.Stop()
    printfn $"took {sw.ElapsedMilliseconds} ms; answer: {solution.Length}"


)