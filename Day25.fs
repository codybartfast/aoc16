open System

type Regs = { A: int64; B: int64; C: int64; D: int64; Out: int64 list }
let zregs = { A = 0L; B = 0L; C = 0L; D = 0L; Out = [] }

let lines = IO.File.ReadAllLines("Day25.txt")

let value regs = function
    | "a" -> regs.A
    | "b" -> regs.B
    | "c" -> regs.C
    | "d" -> regs.D
    | ds -> int64 ds

let cpy src dst (regs, inst) =
    let v = value regs src
    let regs =
        match dst with
        | "a" -> {regs with A = v}
        | "b" -> {regs with B = v}
        | "c" -> {regs with C = v}
        | "d" -> {regs with D = v}
        | _ -> failwithf "Unknown reg: %s" dst
    (regs, inst + 1)

let jnz src rjump (regs, inst) =
    let v = value regs src
    if v = 0L then
        (regs, inst + 1)
    else
        (regs, inst + int rjump)

let inc reg (regs, inst) =
    let regs =
        match reg with
        | "a" -> {regs with A = regs.A + 1L}
        | "b" -> {regs with B = regs.B + 1L}
        | "c" -> {regs with C = regs.C + 1L}
        | "d" -> {regs with D = regs.D + 1L}
        | _ -> failwithf "Unknown reg: %s" reg
    (regs, inst + 1)

let dec reg (regs, inst) =
    let regs =
        match reg with
        | "a" -> {regs with A = regs.A - 1L}
        | "b" -> {regs with B = regs.B - 1L}
        | "c" -> {regs with C = regs.C - 1L}
        | "d" -> {regs with D = regs.D - 1L}
        | _ -> failwithf "Unknown reg: %s" reg
    (regs, inst + 1)

let out x (regs, inst) =
    let regs = {regs with Out = (value regs x)::regs.Out}
    (regs, inst + 1)

let instrs = lines |> Array.map (fun ln ->
    let words = ln.Split(' ')
    match words.[0] with
    | "cpy" -> cpy words.[1] words.[2]
    | "jnz" -> jnz words.[1] words.[2]
    | "inc" -> inc words.[1]
    | "dec" -> dec words.[1]
    | "out" -> out words.[1]
    | _ -> failwithf "Unknown instruction: %s" words.[0])

let rec isvalid = function
    | [_] -> true
    | 0L::1L::tail -> isvalid (1L::tail)
    | 1L::0L::tail -> isvalid (0L::tail)
    | _ -> false

let test len a =
    let rec run instrs (regs, inst) =
        if regs.Out.Length < len && inst < Array.length instrs then
            run instrs (instrs.[inst] (regs, inst))
        else
            regs
    let regs = (run instrs ({zregs with A = a}, 0))
    if isvalid regs.Out then Some a else None

[<EntryPoint>]
let main argv =
    Seq.initInfinite int64
    |> Seq.pick (test 16)
    |> printfn "Part 1: %d"

    "Merry Christmas!"
    |> printfn "Part 2: %s"
    0
