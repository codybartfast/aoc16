open System
open Grid

let rec permutations list =
    let rec insAlong i list =
        match list with
        | [] -> [[i]]
        | h::t -> (i::list)::(List.map (fun sub -> h::sub) (insAlong i t))
    match list with
    | [] -> [[]]
    | head::tail -> List.collect (insAlong head) (permutations tail)

let lines = System.IO.File.ReadAllLines("Day24.txt")
let map = lines |> Grid.ofLines
let locations = map.Filter Char.IsDigit |> Seq.sortBy snd |> Seq.toList
let start = locations.Head |> snd

let disttab =
    let rec distances (map: Grid<char>) steps frontier = seq{
        if List.isEmpty frontier then () else
        yield!
            frontier
            |> List.filter (snd >> Char.IsDigit)
            |> List.map (fun (crd, chr) -> chr, steps)
        let newplaces =
            frontier
            |> List.filter (snd >> (fun c -> (c = '.') || Char.IsDigit c))
            |> List.map fst
        newplaces |> List.iter (fun crd -> map.Set(crd, 'X'))
        let newfrontier =
            newplaces
            |> List.collect (fun crd -> map.AdjacentUC(crd))
            |> List.distinct
        yield! distances map (steps + 1) newfrontier}
    locations
    |> Seq.collect(fun (start) ->
        distances (Grid.ofLines lines) 0 [start]
        |> Seq.map (fun (loc', dst) -> ((snd start, loc'), dst)))
    |> Map

let allroutes =
    permutations (locations.Tail |> List.map snd)
    |> List.map (fun lst -> start::lst)

let routelen = List.pairwise >> List.sumBy (fun pair -> disttab.[pair])

[<EntryPoint>]
let main argv =
    allroutes
    |> List.map routelen
    |> List.min
    |> printfn "Part 1: %A"

    allroutes
    |> List.map ((fun lst -> start::(List.rev lst)) >> routelen)
    |> List.min
    |> printfn "Part 2: %A"
    0
