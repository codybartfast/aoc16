open System
open System.Security.Cryptography
open System.Text

let toHex = (BitConverter.ToString
            >> (fun str ->str.Replace("-", String.Empty)))

let doorid = IO.File.ReadAllText("Day05.txt")
let md5 = MD5.Create()

let hexChar2int = function
    | d when '0' <= d && d <= '9' -> d |> string |> int
    | 'A' | 'a' -> 10
    | 'B' | 'b' -> 11
    | 'C' | 'c' -> 12
    | 'D' | 'd' -> 13
    | 'E' | 'e' -> 14
    | 'F' | 'f' -> 15
    | c -> failwithf "Non hex char: %c" c

let hashstat n =
    let hash = 
        doorid + n.ToString()
        |> Encoding.ASCII.GetBytes
        |> md5.ComputeHash
        |> toHex |> (fun s -> s.ToCharArray())
    match hash |> Array.takeWhile ((=) '0') |> Array.length with
    | n when n >= 5 -> Some (hexChar2int hash.[5], hash.[6])
    | _ -> None

let simple () =
    Seq.initInfinite id
    |> Seq.map hashstat
    |> Seq.choose id
    |> Seq.map snd
    |> Seq.take 8
    |> Seq.toArray |> String

let advanced () =
    let rec iter (pwd: char[]) remaining n =
        match hashstat n with
        | Some (pos, vl) when pos >= 0 && pos < 8 && pwd.[pos] = 'X' ->
            pwd.[pos] <- vl
            match remaining with
            | 1 -> pwd
            | _ -> iter pwd (remaining - 1) (n + 1)
        | _ -> iter pwd remaining (n + 1)
    iter [| 'X'; 'X'; 'X'; 'X'; 'X'; 'X'; 'X'; 'X'; |] 8 0 |> String

[<EntryPoint>]
let main argv =
    simple ()
    |> printfn "Part 1: %A"

    advanced ()
    |> printfn "Part 2: %A"
    0
