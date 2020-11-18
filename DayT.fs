open System
open System.Text.RegularExpressions

let lines = System.IO.File.ReadAllLines("Day2.txt")



[<EntryPoint>]
let main argv =
    lines
    |> printfn "Part 1: %A"

    "?"
    |> printfn "Part 2: %A"
    0
