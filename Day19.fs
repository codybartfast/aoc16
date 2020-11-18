type Elf = { Id: int; mutable Left: Elf }

let nelves = System.IO.File.ReadAllLines("Day19.txt").[0] |> int

let circle n =
    let rec tmp = {Id = 0; Left = tmp }
    let elves, last =
        (tmp, seq{ n .. -1 .. 1})
        ||> Seq.mapFold (fun lft id ->
            let e = {Id = id; Left = lft}
            (e, e))
    let first = Seq.head elves
    first.Left <- last
    last

let rec game1 elf =
    let left = elf.Left
    if elf.Id = left.Id then elf.Id else
    elf.Left <- left.Left
    game1 elf.Left

let rec game2 n btgt =
    let even n = n % 2 = 0
    if n = 1 then btgt.Id else
    btgt.Left <- btgt.Left.Left
    game2 (n - 1) (if n % 2 =0 then btgt else btgt.Left)

let rec advance n elf = match n with  0 -> elf | n -> advance (n - 1) elf.Left

[<EntryPoint>]
let main argv =
    circle nelves
    |> game1
    |> printfn "Part 1: %A"

    circle nelves
    |> (advance (nelves / 2 - 1))
    |> (game2 nelves)
    |> printfn "Part 2: %A"
    0
