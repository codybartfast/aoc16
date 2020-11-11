open System
open System.Text.RegularExpressions

let text = IO.File.ReadAllText("Day2.txt")
let lines = text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)

[<EntryPoint>]
let main argv =
    lines
    |> printfn "Part 1: %A"

    "?"
    |> printfn "Part 2: %A"
    0
