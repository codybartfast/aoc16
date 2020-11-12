#nowarn "25"
open System
open System.Text.RegularExpressions

open Grid

let lines = IO.File.ReadAllLines("Day08.txt")

type Lights = Grid<char>

let rect a b (lights: Lights) =
    for y in 0 .. (b - 1) do
        for x in 0 .. (a - 1) do
            lights.[x, y] <- '#'
    lights
let rotrow a b (lights: Lights)=
    let row = lights.Row(a)
    for x in 0 .. row.Length - 1 do
        lights.[(x + b) % row.Length, a] <- row.[x]
    lights
let rotcol a b (lights: Lights)=
    let col = lights.Col(a)
    for y in 0 .. col.Length - 1 do
        lights.[a, (y + b) % col.Length] <- col.[y]
    lights

let insts = lines |> Array.map(fun ln ->
    let [| a; b |] =
        Regex.Matches(ln, @"(\d+)")
        |> Seq.map (fun m -> m.Value |> int)
        |> Seq.toArray
    if ln.StartsWith("rect") then rect a b
    elif ln.StartsWith("rotate row") then rotrow a b
    else rotcol a b )

[<EntryPoint>]
let main argv =
    let lights = Grid.initWith 50 6 '.'

    (lights, insts)
    ||> Seq.fold (fun grid inst -> inst grid)
    |> Grid.count ((=) '#')
    |> printfn "Part 1: %A"

    lights // AocFont!
    |> Grid.astextf fchar
    |> printfn "Part 2: \n%s"
    0
