open System
#nowarn "25"

let initial = IO.File.ReadAllLines("Day16.txt").[0].ToCharArray()

let prime dsk = initial |> Array.iteri (fun i a -> Array.set dsk i a); dsk

let rec expand used (dsk: char[])=
    match used with
    | u when u = dsk.Length -> dsk
    | _ ->
        dsk.[used] <- '0'
        let fill = min used (dsk.Length - 1 - used)
        seq { 1 .. fill }
        |> Seq.iter (fun i ->
            dsk.[used + i] <- (function '0' -> '1' | '1' -> '0') dsk.[used - i])
        expand (used + 1 + fill) dsk

let rec chksum (data: char []) =
    let fn [|a; b|] = match (a = b) with true -> '1' | _ -> '0'
    if data.Length % 2 = 1 then String data else
    data |> Array.chunkBySize 2 |> Array.map fn |> chksum

let overwrite = Array.zeroCreate >> prime >> expand initial.Length >> chksum

[<EntryPoint>]
let main argv =
    overwrite 272 |> printfn "Part 1: %A"
    overwrite 35651584 |> printfn "Part 2: %A"
    0
