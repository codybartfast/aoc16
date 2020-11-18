open System

let lines = System.IO.File.ReadAllLines("Day21.txt")

let swapPosition p1 p2 =
    fun (arr: char[]) ->
        let tmp = arr.[p1]
        arr.[p1] <- arr.[p2]
        arr.[p2] <- tmp
        arr

let swapLetter c1 c2 =
    Array.map (fun c -> if c = c1 then c2 elif c = c2 then c1 else c)

let rotateLeft n =
    fun (arr: char[]) ->
        let src = Array.copy arr
        let len = arr.Length
        let n = len + n
        [| 0 .. len - 1 |]
        |> Array.map (fun i -> src.[(i + n) % len])

let rotateBased c =
    let lookup = [| 1; 2; 3; 4; 6; 7; 0; 1 |]
    fun (arr: char[]) ->
        assert (arr.Length = 8)
        let idx = Array.findIndex ((=) c) arr
        rotateLeft (- lookup.[idx]) arr

let iRotateBased c =
    let lookup = [| 1; 1; 6; 2; 7; 3; 0; 4 |]
    fun (arr: char[]) ->
        assert (arr.Length = 8)
        let idx = Array.findIndex ((=) c) arr
        rotateLeft lookup.[idx] arr

let reversePositions p1 p2 =
    fun (arr: char[]) ->
        (arr, [0 .. (p2 - p1) / 2])
        ||> List.fold (fun arr i ->
            let p1, p2 = p1 + i, p2 - i
            let tmp = arr.[p1]
            arr.[p1] <- arr.[p2]
            arr.[p2] <- tmp
            arr)

let movePosition n1 n2 =
    assert (n1 <> n2) |> ignore
    fun (arr: char[]) ->
        let tmp = arr.[n1]
        let inc = if n1 < n2 then 1 else -1
        [n1 .. inc .. (n2 - inc)]
        |> List.iter (fun i -> arr.[i] <- arr.[i + inc])
        arr.[n2] <- tmp
        arr

let action (ln: string) =
    let wds = ln.Split(' ')
    match wds.[0], wds.[1] with
    | "swap", "position" -> swapPosition (int wds.[2]) (int wds.[5])
    | "swap", "letter" -> swapLetter wds.[2].[0] wds.[5].[0]
    | "rotate", "left" -> rotateLeft (int wds.[2])
    | "rotate", "right" -> rotateLeft (- (int wds.[2]))
    | "rotate", "based" -> rotateBased wds.[6].[0]
    | "reverse", "positions" -> reversePositions (int wds.[2]) (int wds.[4])
    | "move", "position" -> movePosition (int wds.[2]) (int wds.[5])
    | pair -> failwithf "Unknown action %A" pair

let inverse (ln: string) =
    let wds = ln.Split(' ')
    match wds.[0], wds.[1] with
    | "swap", "position" -> swapPosition (int wds.[2]) (int wds.[5])
    | "swap", "letter" -> swapLetter wds.[2].[0] wds.[5].[0]
    | "rotate", "left" -> rotateLeft (- (int wds.[2]))
    | "rotate", "right" -> rotateLeft (int wds.[2])
    | "rotate", "based" -> iRotateBased wds.[6].[0]
    | "reverse", "positions" -> reversePositions (int wds.[2]) (int wds.[4])
    | "move", "position" -> movePosition (int wds.[5]) (int wds.[2])
    | pair -> failwithf "Unknown inversion %A" pair

let actions = lines |> Array.map action
let inversions = lines |> Array.rev |> Array.map inverse

let apply tforms (str: string) =
    (str.ToCharArray(), tforms)
    ||> Array.fold (fun arr tform -> tform arr)
    |> String

[<EntryPoint>]
let main argv =
    apply actions "abcdefgh" |> printfn "Part 1: %s"
    apply inversions "fbgdceah" |> printfn "Part 2: %s"
    0
