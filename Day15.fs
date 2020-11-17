open System
open System.Text.RegularExpressions

let discs =
    IO.File.ReadAllLines("Day15.txt")
    |> Array.map (fun ln ->
        let digits = Regex.Split(ln, @"\D+")
        int digits.[2], int digits.[4])
    |> Array.toList

let rec align period time discs =
    let advance (size, pstn) time = (size, (pstn + time) % size)
    match discs with
    | [] -> time - 1
    | disc::rest ->
        match advance disc time with
        | (size, 0) -> align (period * size) (time + 1) rest
        | _ -> align period (time + period) discs

let starttime discs = discs  |> (align 1 1) |> (fun t -> t - discs.Length)

[<EntryPoint>]
let main argv =
    discs |> starttime |> printfn "Part 1: %A"
    discs @ [11, 0] |> starttime |> printfn "Part 2: %A"
    0
