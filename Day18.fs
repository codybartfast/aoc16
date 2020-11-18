let toprow = System.IO.File.ReadAllLines("Day18.txt").[0]
let len = toprow.Length

let rset (row: char[]) =
    let ri = (function -1 -> '.' | i when i = len -> '.' | i -> row.[i])
    (fun i -> [| i - 1; i; i + 1 |] |> Array.map ri)

let newtile = function
    | [| '^'; '^'; '.' |] | [| '.'; '^'; '^' |]
    | [| '^'; '.'; '.' |] | [| '.'; '.'; '^' |]  -> '^'
    | _ -> '.'

let newrow row = [| 0 .. len - 1 |] |> Array.map (rset row >> newtile)

let countsafe (row0: string) nrows =
    row0.ToCharArray()
    |> Seq.unfold (fun r -> Some (r, newrow r))
    |> Seq.take nrows
    |> Seq.sumBy (Seq.filter ((=) '.') >> Seq.length)

[<EntryPoint>]
let main argv =
    countsafe toprow 40 |> printfn "Part 1: %A"
    countsafe toprow 400000 |> printfn "Part 2: %A"
    0
