type PuzzleInput =
    { OrderRules: (int * int) list
      Updates: int list list }

module PuzzleInput =

    type Entry =
        | OrderRule of (int * int)
        | Update of int list

    let parse data  =
        let entries =
            data
            |> List.choose (
            Array.ofSeq
            >> function
                | [| c1; c2; '|'; c3; c4 |] ->
                    let n1 = int (System.String([| c1; c2 |]))
                    let n2 = int (System.String([| c3; c4 |]))
                    Some(OrderRule(n1, n2))
                | [||] -> None
                | other ->
                    Some(
                        Update(
                            other
                            |> System.String
                            |> (fun x -> x.Split ',')
                            |> Array.map int
                            |> List.ofArray
                        )
                    )
        )
            
        {
            OrderRules =
                entries
                |> List.choose(
                    function
                    |OrderRule v -> Some v
                    | _ -> None)
            Updates =
                entries
                |> List.choose (
                    function
                    |Update v -> Some v
                    | _ -> None)
        }
        

module OrderRule =
    
    let followingMap values =
        values
        |> List.groupBy fst
        |> List.map (
            fun (k, values) ->  k, (values |> List.map snd))
        |> Map.ofList
        

module Update =
    let valuesWithPreceding (values: int list) =
        ([], values) ||> List.fold(
            fun preceding value ->
                match preceding |> List.tryHead with
                | Some (last, precedingLast) ->
                    (value, last :: precedingLast) :: preceding
                | None -> [ (value, []) ]
            )
    
    let isCorrect (mustFollowMap: Map<int, int list>) =
        valuesWithPreceding
        >> List.forall (fun (v, preceding) ->
            match mustFollowMap |> Map.tryFind v with
            | Some mustFollow ->
                preceding |> List.forall (fun v ->not (List.contains v mustFollow))
            | None -> true
            )


let solveDay1  input =
    let map = OrderRule.followingMap input.OrderRules
    input.Updates |> List.filter (Update.isCorrect map) |> List.map(fun x -> x[x.Length/2]) |> List.sum
    
let puzzleInput =
    "day5/input.txt"
    |> System.IO.File.ReadAllLines
    |> List.ofArray
    |> PuzzleInput.parse
    
solveDay1 puzzleInput