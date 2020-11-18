let lines = System.IO.File.ReadAllLines("Day20.txt")
let ranges =
    lines
    |> Array.map (fun ln ->
        let wrds = ln.Split('-')
        (uint wrds.[0], uint wrds.[1]) )
    |> Array.sortBy fst
    |> Array.toList

let rec simplify ranges (first, last) simplified =
    match ranges with
    | [] -> (first, last)::simplified |> List.rev
    | (first', last')::ranges when (first' - 1u) <= last ->
        if last' > last then
            simplify ranges (first, last') simplified
        else
            simplify ranges (first, last) simplified
    | range::ranges -> simplify ranges range ((first, last)::simplified)

let rec invert last ranges inv =
    match ranges with
    | [] -> inv |> List.rev
    | (first', last')::ranges ->
        invert last' ranges ((last + 1u, first' - 1u)::inv)

[<EntryPoint>]
let main argv =
    let sranges = simplify ranges.Tail ranges.Head []

    sranges.Head |> snd |> ((+) 1u)
    |> printfn "Part 1: %d"

    invert (sranges.Head |> snd) sranges.Tail []
    |> List.sumBy (fun (f, l)  -> 1u + f - l)
    |> printfn "Part 2: %d"
    0
