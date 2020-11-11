open System
#nowarn "25"

let text = IO.File.ReadAllText("Day01.txt")
let words = text.Split(", ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
let insts = words |> Array.map (fun wd ->
    wd.[0], wd.Substring(1) |> int)

type Points = North | East | South | West

let turn point trn =
    match point, trn with
    | North, 'L' -> West | North, 'R' -> East
    | West, 'L' -> South | West, 'R' -> North
    | South, 'L' -> East | South, 'R' -> West
    | East, 'L' -> North | East, 'R' -> South
    | _ -> failwith "oops"

let move (x, y) pt dist =
    match pt with
    | North -> (x + dist, y)
    | East -> (x, y + dist)
    | South -> (x - dist, y)
    | West -> (x, y - dist)

let jump (pos, pt) (trn, dist) =
    let pt = turn pt trn
    let pos = move pos pt dist
    (pos, pt)

let walk (pos, pt, been, first) (trn, dist) =
    let pt = turn pt trn
    ((pos, pt, been, first), [1 .. dist])
    ||> Seq.scan (fun (pos, pt, been, first) _ ->
        match first with
        | Some _ -> (pos, pt, been, first)
        | _ ->
            let pos = move pos pt 1
            match Set.contains pos been with
            | true -> (pos, pt, been, Some pos)
            | _ -> (pos, pt, been.Add pos, None))
    |> Seq.last

let distance (x, y) = abs x + abs y

[<EntryPoint>]
let main argv =
    insts
    |> Seq.fold jump ((0, 0), North)
    |> fst |> distance
    |> printfn "Part 1: %A"

    insts
    |> Seq.scan walk ((0, 0), North, Set.singleton (0, 0), None)
    |> Seq.last
    |> (fun (_, _, _, Some pos) -> pos)
    |> distance
    |> printfn "Part 2: %A"
    0
