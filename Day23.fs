#nowarn "25"
let lines = System.IO.File.ReadAllLines("Day23.txt")

type Regs = { A: int64; B: int64; C: int64; D: int64 }
let zregs = { A = 0L; B = 0L; C = 0L; D = 0L }

let inbounds n arr = 0 <= n && n < Array.length arr
let inbounds64 n arr =
    let n= int n
    0 <= n && n < Array.length arr

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
    (regs, inst + 1)

let jnz src rjump (regs, inst) =
    let v = value regs src
    if v = 0L then
        (regs, inst + 1)
    else
        (regs, inst + ((value regs rjump) |> int))

let inc reg (regs, inst) =
    let regs =
        match reg with
        | "a" -> {regs with A = regs.A + 1L}
        | "b" -> {regs with B = regs.B + 1L}
        | "c" -> {regs with C = regs.C + 1L}
        | "d" -> {regs with D = regs.D + 1L}
    (regs, inst + 1)

let dec reg (regs, inst) =
    let regs =
        match reg with
        | "a" -> {regs with A = regs.A - 1L}
        | "b" -> {regs with B = regs.B - 1L}
        | "c" -> {regs with C = regs.C - 1L}
        | "d" -> {regs with D = regs.D - 1L}
    (regs, inst + 1)

let tgl instr =
    match instr with
    | [| "inc"; a1 |] -> [| "dec"; a1 |]
    | [| _; a1 |] -> [| "inc"; a1 |]
    | [| "jnz"; a1; a2 |] -> [| "cpy"; a1; a2 |]
    | [| _; a1; a2 |] -> [| "jnz"; a1; a2 |]

let instrs () = lines |> Array.map (fun ln ->
    let words = ln.Split(' ')
    if words.Length = 3 then
        [| words.[0]; words.[1]; words.[2] |]
    else
        [| words.[0]; words.[1]; |] )

let rec run instrs (regs, inst) =
    match inbounds inst instrs with
    | false -> regs
    | true ->
        let regs, inst =
            match instrs.[inst] with
            | [| "cpy"; src; dest |] -> cpy src dest (regs, inst)
            | [| "jnz"; src; rjump |] -> jnz src rjump (regs, inst)
            | [| "inc"; reg; |] -> inc reg (regs, inst)
            | [| "dec"; reg; |] -> dec reg (regs, inst)
            | [| "tgl"; rinst; |] ->
                let tinst = (int64 inst) + (value regs rinst)
                if inbounds64 tinst instrs then
                    instrs.[int tinst] <- tgl instrs.[int tinst]
                (regs, inst + 1)
        run instrs (regs, inst)

[<EntryPoint>]
let main argv =
    let regs = run (instrs ()) ({zregs with A = 7L}, 0)
    printfn "Part 1: %d" regs.A

    printfn "Part 2: (Put the kettle on ...)"
    let regs = run (instrs ()) ({zregs with A = 12L}, 0)
    printfn "%d" regs.A
    0
