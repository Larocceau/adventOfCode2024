let w, h = 10, 10
let p = 15

let person =
    let x = [ 0..w ] |> List.randomChoice
    let y = [ 0..h ] |> List.randomChoice
    x, y

let map =
    [ for y in [ 0..h ] do
          System.String
              [| for x in [ 0..w ] do
                     if person = (x, y) then '^'
                     else if System.Random.Shared.Next(100) < p then '#'
                     else '.'

                 |]

      ]

System.IO.File.WriteAllLines("generated.txt", map)
