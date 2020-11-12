#nowarn "25"
open System

let lines = IO.File.ReadAllLines("Day09.txt")
let input = lines.[0].ToCharArray() |> Array.toList

type Ver = V1 | V2

let header src =
    let openparan = function '('::src-> src
    let closeparan = function ')'::src-> src
    let by = function 'x'::src-> src
    let num src =
        let rec iter src digits =
            match src with
            | d::src when '0' <= d && d <= '9' -> iter src (d::digits)
            | _ -> digits |> List.rev |> List.toArray |> String |> int64, src
        iter src []
    let len, src = src |> openparan |> num
    let rpt, src = src |> by |> num
    len, rpt, src |> closeparan

let rec simplen ver src = seq{
    match src with
    | [] -> ()
    | '('::_ -> yield! decomp ver src
    | _::src ->
        yield 1L
        yield! simplen ver src }
and decomp ver src = seq{
    let len, rpt, src = header src
    let section = src |> List.take (len |> int)
    let src = src |> List.skip (len |> int)
    yield rpt * (match ver with V1 -> len | V2 -> (declen ver section))
    yield! simplen ver src }
and declen ver src = simplen ver src |> Seq.sum

[<EntryPoint>]
let main argv =
    declen V1 input |> printfn "Part 1: %A"
    declen V2 input |> printfn "Part 2: %A"
    0
