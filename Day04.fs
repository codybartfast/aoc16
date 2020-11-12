open System
open System.Text.RegularExpressions

let lines = IO.File.ReadAllLines("Day04.txt")
let rooms = lines |> Array.map (fun ln ->
    let wrds = Regex.Split(ln, @"(?<=\D)(?=\d)|\[|\]")
    wrds.[0], int wrds.[1], wrds.[2])
let destination = "northpole"

let chksum (enc: string) =
    enc.ToCharArray()
    |> Seq.countBy id
    |> Seq.sortBy (fun (ch, n) -> -n, ch)
    |> Seq.map fst
    |> Seq.take 5
    |> Seq.toArray |> String

let isreal (rm: string, _, cs) = chksum (rm.Replace("-", "")) = cs

let decrypt key (rm: string) =
    rm.ToCharArray()
    |> Array.map (function
        | '-' -> ' '
        | c -> ((int c) - (int 'a') + key) % 26 + (int 'a') |> char)
    |> String

[<EntryPoint>]
let main argv =
    rooms
    |> Seq.filter isreal
    |> Seq.sumBy (fun (_, id, _) -> id)
    |> printfn "Part 1: %A"

    rooms
    |> Seq.filter isreal
    |> Seq.find (fun (rm, id, _) -> (decrypt id rm).Contains(destination))
    |> fun (_, id, _) -> id
    |> printfn "Part 2: %A"
    0
