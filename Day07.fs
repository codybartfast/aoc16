open System
open System.Text.RegularExpressions

let addresses = IO.File.ReadAllLines("Day07.txt")

let hasAbba str = Regex.IsMatch(str, @"(?<=^|\])\w*(\w)(?!\1)(\w)\2\1")
let hasHyper str = Regex.IsMatch(str, @"(?<=\[)\w*(\w)(?!\1)(\w)\2\1")
let hasTls str = hasAbba str && not (hasHyper str)

let hasSsl str =
    Regex.IsMatch(str, @"(?<=^\w*|\]\w*)(\w)(?!\1)(\w)\1.*(?<=\[\w*)\2\1\2")
    || Regex.IsMatch(str, @"(?<=\[\w*)(\w)(?!\1)(\w)\1.*(?<=\]\w*)\2\1\2")

[<EntryPoint>]
let main argv =
    addresses
    |> Seq.filter hasTls |> Seq.length
    |> printfn "Part 1: %A"

    addresses
    |> Seq.filter hasSsl |> Seq.length
    |> printfn "Part 2: %A"
    0
