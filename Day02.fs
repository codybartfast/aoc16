open System
#nowarn "25"

let text = IO.File.ReadAllText("Day02.txt")
let lines = text.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
let sequences = lines |> Array.map (fun ln -> ln.ToCharArray() )

let nextdigit loc sequence =
    let move (x, y) dir =
        let xmove x dir =
            match x, dir with
            | 0, 'L' -> 0 | 1, 'L' -> 0 | 2, 'L' -> 1
            | 0, 'R' -> 1 | 1, 'R' -> 2 | 2, 'R' -> 2
            | n, _ -> n
        let ymove y dir =
            match y, dir with
            | 0, 'U' -> 0 | 1, 'U' -> 0 | 2, 'U' -> 1
            | 0, 'D' -> 1 | 1, 'D' -> 2 | 2, 'D' -> 2
            | n, _ -> n
        (xmove x dir), (ymove y dir)
    Seq.scan move loc sequence |> Seq.last

let digit = function
    | (0, 0) -> '1' | (1, 0) -> '2' | (2, 0) -> '3'
    | (0, 1) -> '4' | (1, 1) -> '5' | (2, 1) -> '6'
    | (0, 2) -> '7' | (1, 2) -> '8' | (2, 2) -> '9'

let number sequences =
    ((1, 1), sequences)
    ||> Seq.scan nextdigit
    |> Seq.tail
    |> Seq.map digit
    |> Seq.toArray |> String


let nextchar loc sequence =
    let move a dir  =
        match a, dir with
        | '1', 'D' -> '3'
        | '2', 'R' -> '3' | '2', 'D' -> '6'
        | '3', 'U' -> '1' | '3', 'R' -> '4' | '3', 'D' -> '7' | '3', 'L' -> '2'
        | '4', 'L' -> '3' | '4', 'D' -> '8'
        | '5', 'R' -> '6'
        | '6', 'U' -> '2' | '6', 'R' -> '7' | '6', 'D' -> 'A' | '6', 'L' -> '5'
        | '7', 'U' -> '3' | '7', 'R' -> '8' | '7', 'D' -> 'B' | '7', 'L' -> '6'
        | '8', 'U' -> '4' | '8', 'R' -> '9' | '8', 'D' -> 'C' | '8', 'L' -> '7'
        | '9', 'L' -> '8'
        | 'A', 'U' -> '6' | 'A', 'R' -> 'B'
        | 'B', 'U' -> '7' | 'B', 'R' -> 'C' | 'B', 'D' -> 'D' | 'B', 'L' -> 'A'
        | 'C', 'U' -> '8' | 'C', 'L' -> 'B'
        | 'D', 'U' -> 'B'
        | c, _ -> c
    Seq.scan move loc sequence |> Seq.last

let code sequences =
    ('5', sequences)
    ||> Seq.scan nextchar
    |> Seq.tail
    |> Seq.toArray |> String

[<EntryPoint>]
let main argv =
    number sequences
    |> printfn "Part 1: %A"

    code sequences
    |> printfn "Part 2: %A"
    0
