open System
open System.Text
open System.Text.RegularExpressions
open System.Security.Cryptography

let salt = IO.File.ReadAllLines("Day14.txt").[0]

let md5 = MD5.Create()

let toHex =
    BitConverter.ToString
    >> (fun str -> str.Replace("-", String.Empty).ToLower())
let hash (str: string) =
    str
    |> Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> toHex

let stretch rounds n =
    (salt + n.ToString(), seq{1 .. rounds})
    ||> Seq.fold (fun str _ -> hash str)

let istripp rounds n =
    match Regex.Match(stretch rounds n, @"(.)\1\1") with
    | m when m.Success -> Some (n, m.Value.[0])
    | _ -> None

let quint rounds n =
    match Regex.Match(stretch rounds n, @"(.)\1\1\1\1") with
    | m when m.Success -> Some (n, m.Value.[0])
    | _ -> None

let rec quints (rounds: int) (known: (int * char ) list) n =
    let rec from n = seq{ yield n; yield! from (n + 1) }
    let max = match known with [] -> -1 | (max, _)::_ -> max
    if max >= n then
        known
    else
        quints rounds ((from (max + 1) |> Seq.pick (quint rounds))::known) n

let rec keys rounds knownquints (n: int) = seq {
    match istripp rounds n with
    | None -> yield! keys rounds knownquints (n + 1)
    | Some (n, chr) ->
        let knownquints = quints rounds knownquints (n + 1000)
        let foundquint =
            knownquints
            |> List.skipWhile (fst >> ((<) (n + 1000)))
            |> List.takeWhile (fst >>  ((<) n))
            |> List.exists (snd >> ((=) chr))
        match foundquint with
        | false -> ()
        | true -> yield n, salt + n.ToString()
        yield! keys rounds knownquints (n + 1) }

[<EntryPoint>]
let main argv =
    keys 1 [] 0
    |> Seq.item 63
    |> printfn "Part 1: %A"

    keys 2017 [] 0
    |> Seq.item 63
    |> printfn "Part 2: %A"
    0
