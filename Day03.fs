open System
open System.Text.RegularExpressions

let lines = IO.File.ReadAllLines("Day03.txt")
let shapes = lines |> Array.map (fun ln ->
    Regex.Split(ln.Trim(), @"\s+") |> Array.map int )

let nTriangles =
    let isTriangle (shape: int[]) = shape.[0] + shape.[1] > shape.[2]
    Array.map Array.sort >> Array.filter isTriangle >> Array.length

let transform = Array.chunkBySize 3 >> Array.collect Array.transpose

[<EntryPoint>]
let main argv =
    shapes |> nTriangles
    |> printfn "Part 1: %A"

    shapes |> transform |> nTriangles
    |> printfn "Part 2: %A"
    0
