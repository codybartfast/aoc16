#nowarn "25"
open System
open System.Text.RegularExpressions

type Kind = Bot | Out | Ans
let kind = function "bot" -> Bot | "output" -> Out

type Rule = {Chip: int; Low: Kind * int; High: Kind * int}
type Loc = Kind * int
type State = Map<Loc ,Set<int>>

let lines = IO.File.ReadAllLines("Day10.txt")

let [| ruleLns; valueLns |] =
    lines |> Array.groupBy (fun ln -> ln.Split(' ').[0]) |> Array.map snd

let rules =
    ruleLns
    |> Array.map(fun ln ->
        let wrds = ln.Split(" ")
        let chip = wrds.[1] |> int
        (chip, { Chip = chip;
                Low = wrds.[5] |> kind, wrds.[6] |> int;
                High = wrds.[10] |> kind, wrds.[11] |> int; }) )
    |> Map.ofArray

let addchip (state: State) loc chip =
    state.Add (loc,
        match Map.tryFind loc state with
        | Some chips ->  chips.Add(chip)
        | None -> Set.singleton chip)

let state =
    valueLns
    |> Array.map(fun ln ->
        Regex.Split(ln, @"\D+") |> (fun [|_; c; d|] -> ((Bot, int d), int c)))
    |> Array.fold (fun state (l, c) -> addchip state l c) Map.empty
let start =
    state |> Map.toList |> List.find (snd >> Set.count >> ((<) 1)) |> fst

let clear state loc = Map.add loc Set.empty state
let twochips = Set.count >> ((=) 2)
let isbot = fst >> ((=) Bot)
let setans state loc = (addchip state (Ans, 0) (snd loc))
let getans (state: State) = state.[(Ans, 0)] |> Set.toSeq |> Seq.exactlyOne

let rec give state loc chip =
    let state = addchip state loc chip
    if twochips state.[loc] && isbot loc then apply state loc else state
and apply (state: State) (loc: Loc) =
    let rule = rules.[snd loc]
    let [lowchp; howchp] = state.[loc] |> Set.toList |> List.sort
    let state = if lowchp = 17 && howchp = 61 then setans state loc else state
    let state = give state rule.Low lowchp
    let state = give state rule.High howchp
    (clear state loc)

[<EntryPoint>]
let main argv =
    let state = apply state start

    getans state
    |> printfn "Part 1: %A"

    state
    |> Map.toSeq
    |> Seq.filter (fst >> (fun (k, n) -> k = Out && n < 3))
    |> Seq.map (snd >> Set.toSeq >> Seq.exactlyOne)
    |> Seq.reduce (*)
    |> printfn "Part 2: %A"
    0
