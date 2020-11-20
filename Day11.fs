open System
open System.Text.RegularExpressions

type Kind = G | M
type Part = (string * Kind)
type State = int * (Part list list)

let lines = System.IO.File.ReadAllLines("Day11.txt")

let parse ln =
    List.append
        (Regex.Matches(ln, @" (\w+)-compatible ")
            |> Seq.map (fun m -> m.Groups.[1].Value, M)
            |> Seq.toList)
        (Regex.Matches(ln, @" (?<gen>\w+) generator")
            |> Seq.map (fun m ->  m.Groups.[1].Value, G)
            |> Seq.toList)
    |> List.sort
let initialState = 0, lines |> Array.toList |> List.map parse
let elvFloor (s: State) = s |> fst
let floors (s: State) = s |> snd
let curFloor (s: State) = List.item (elvFloor s) (floors s)

let isSafe (floor: Part list) =
    let haveGen = floor |> List.exists (snd >> ((=) G))
    let loneMicro =
        floor
        |> List.groupBy fst
        |> List.exists (function (_, [_, M]) -> true | _ -> false)
    not (loneMicro && haveGen)

let rec potLoads (parts: Part list) : seq<Part list> =  seq{
    match parts with
        | []  | [_] -> yield! parts |> List.map List.singleton
        | h::tail ->
            yield [h]
            yield! tail |> List.map(fun t -> [h; t])
            yield! potLoads tail }

let potFloors = function 0 -> [1] | 1 -> [0; 2] | 2 -> [1; 3] | _ -> [2]

let move state frmFloor toFloor load =
    let newFloors =
        (floors state)
        |> List.mapi (fun i flr ->
            if i = frmFloor then
                flr |> List.filter (fun prt -> List.contains prt load |> not)
            elif i = toFloor then
                (load @ flr) |> List.sort
            else
                flr)
    let state = (toFloor, newFloors)
    let changedFloors = [newFloors.[frmFloor]; newFloors.[toFloor]]
    (state, changedFloors)

let potStates state =
    let floor = elvFloor state
    let loads = state |> curFloor |> potLoads
    potFloors floor
    |> Seq.collect (fun newflr -> loads |> Seq.map (move state floor newflr))
    |> Seq.filter (fun (_, changed) -> changed |> List.forall isSafe)
    |> Seq.map fst

let finished state =
    state |> floors |> List.takeWhile List.isEmpty |> List.length |> ((=) 3)

let rec goFourth history steps states =
    let newstates = Set.difference (Set states) history
    let history = Set.union history newstates
    if newstates |> Seq.exists finished then steps else
    // printfn "%d" newstates.Count
    goFourth history (steps + 1) (newstates |> Seq.collect potStates)

let extraParts = """An elerium generator. An elerium-compatible microchip.
    A dilithium generator. A dilithium-compatible microchip."""

let extend state flr extras =
    (elvFloor state),
        (floors state)
        |> List.mapi (fun i floor ->
            if i = flr then extras @ floor |> List.sort else floor)

[<EntryPoint>]
let main argv =
    goFourth Set.empty 0 [initialState]
    |> printfn "Part 1: %A"

    printfn "\n(Go cook your dinner ...)"
    goFourth Set.empty 0 [extend initialState 0 (parse extraParts)]
    |> printfn "Part 2: %A"
    0
