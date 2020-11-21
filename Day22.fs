open System.Text.RegularExpressions

type Node = { Crd: int * int; Used: int; Avail: int }
type Coord = int * int
type Grid = Map<Coord, Node>
let coord (node: Node) = node.Crd

let lines = System.IO.File.ReadAllLines("Day22.txt") |> Array.skip 2
let parse ln =
    let m = Regex.Match(ln, @"x(\d+)-y(\d+) +(\d+)T +(\d+)T +(\d+)T")
    {   Crd = int m.Groups.[1].Value, int m.Groups.[2].Value
        Used =  int m.Groups.[4].Value
        Avail =  int m.Groups.[5].Value }
let nodes = lines |> Array.map parse
let grid = nodes |> Array.map (fun nd -> nd.Crd, nd) |> Map
let width = nodes |> Seq.map (coord >> fst) |> Seq.distinct |> Seq.length
let height = nodes |> Seq.map (coord >> snd) |> Seq.distinct |> Seq.length

let pickpairs pred =
    nodes
    |> Seq.collect (fun nd1 -> nodes |> Seq.choose (fun nd2 ->
        if pred nd1 nd2 then Some (nd1, nd2) else None))

let viable nodes =
    pickpairs (fun nd1 nd2 -> nd1.Used > 0 && nd1.Used <= nd2.Avail)

let dist {Crd=(x1, y1)} {Crd=(x2, y2)} = abs (x1 - x2) + abs (y1 - y2)

let start =
    (fun nd1 nd2 -> dist nd1 nd2 = 1 && nd1.Used <= nd2.Avail && nd1.Used > 0)
    |> pickpairs |> Seq.map (snd >> coord) |> Seq.distinct |> Seq.exactlyOne

let adjcrds (x, y) =
    [(x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1)]
    |> List.filter (fun (x, y) -> 0 <= x && x < width && 0 <= y && y < height)

let freeable data history (toCrd, grid) =
    let toNd = Map.find toCrd grid
    Set.difference (toCrd |> adjcrds |> Set) history
    |> Seq.map (fun crd -> Map.find crd grid)
    |> Seq.filter 
        (fun frmNd -> frmNd.Used <= toNd.Avail && (coord frmNd) <> data)
    |> Seq.map (fun nd -> (toCrd, grid), nd)
    |> Seq.toList

let move ((toCrd, grid), frmNd) =
    let toNd, frmCrd = Map.find toCrd grid, frmNd.Crd
    let sz = frmNd.Used
    grid
    |> Map.add toCrd {
            toNd with Avail = toNd.Avail - sz; Used = toNd.Used + sz}
    |> Map.add frmCrd {
            frmNd with Avail = frmNd.Avail + sz; Used = frmNd.Used - sz}
    |> fun grid -> frmCrd, grid

let rec free freeCrd datCrd steps history states =
    let crds = states |> Seq.map fst |> Set
    if crds.Contains freeCrd then
        steps, states |> Seq.find (fst >> ((=) freeCrd))
    else
        states
        |> List.collect (freeable datCrd history)
        |> List.groupBy (snd >> coord)
        |> List.map (snd >> List.head >> move)
        |> free freeCrd datCrd (steps + 1) (Set.union history crds)

let rec moveData steps state x =
    if x = 0 then steps else
    let data, next = (x, 0), (x - 1, 0)
    let steps, (_, grid) = free next data steps Set.empty [state]
    let state = move ((next, grid), (grid.[data]))
    moveData (steps + 1) state (x - 1)

[<EntryPoint>]
let main argv =
    viable nodes |> Seq.length |> printfn "Part 1: %A"
    moveData 0 (start, grid) (width - 1) |> printfn "Part 2: %A"
    0
