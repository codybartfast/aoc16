open System

let lines = IO.File.ReadAllLines("Day06.txt")
let cols = lines |> Array.map (fun ln -> ln.ToCharArray()) |> Array.transpose

let mostCommon = Seq.countBy id >> Seq.maxBy snd >> fst
let leastCommon = Seq.countBy id >> Seq.minBy snd >> fst

[<EntryPoint>]
let main argv =
    cols
    |> Array.map mostCommon
    |> String
    |> printfn "Part 1: %A"

    cols
    |> Array.map leastCommon
    |> String
    |> printfn "Part 2: %A"
    0
