open System

let lines = IO.File.ReadAllLines("Day13.txt")
let favnum = int lines.[0]
let target = 31,39

type Sqr =  Wall | Unvisited | Dist of int
type Coord = int * int

let rec int2Bin = function | 0 -> [0] | 1 -> [1] | n -> (n % 2)::int2Bin (n / 2)

let calc (x, y) =
    x*x + 3*x + 2*x*y + y + y*y + favnum
    |> int2Bin
    |> List.sum
    |> (fun n -> n % 2)
    |> function 0 -> Unvisited | 1 -> Wall | _ -> failwith "oops"

let mutable map = Map.empty
let get, set =
    let get coord =
        if fst coord < 0 || snd coord < 0 then Wall else
        if map.ContainsKey coord then map.[coord] else
        map <- map.Add (coord, calc coord)
        map.[coord]
    let set coord n =
        match get coord with
        | Wall -> false
        | Dist _ -> false
        | Unvisited -> map <- map.Add (coord, Dist n); true
    get, set

let update (dist, coord)  =
    match set coord dist with
    | false -> []
    | true ->
        let x, y = coord
        [ (x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1); ]
        |> List.map (fun crd -> (dist + 1, crd))

let rec search trgt frontier =
    let found  =  frontier |> List.map snd |> List.contains trgt
    if found then
        frontier.Head |> fst
    else
        search trgt (frontier |> List.collect update)

[<EntryPoint>]
let main argv =
    search target [(0, (1, 1))]
    |> printfn "Part 1: %A"

    map
    |> Map.toSeq
    |> Seq.choose (snd >> function Dist n -> Some n | _ -> None)
    |> Seq.filter (fun n -> n <= 50)
    |> Seq.length
    |> printfn "Part 2: %A"
    0
