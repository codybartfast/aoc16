open System
open System.Text.RegularExpressions

let text = IO.File.ReadAllText("Day0.txt")
let lines = text.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)

[<EntryPoint>]
let main argv =
    lines
    |> printfn "Part 1: %A"

    "?"
    |> printfn "Part 2: %A"
    0
