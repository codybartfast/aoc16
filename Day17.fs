open System
open System.Security.Cryptography
open System.Text

let passcode = IO.File.ReadAllLines("Day17.txt").[0]
let origin = ((0, 0), passcode)

let hash : string -> string =
    let md5 = MD5.Create()
    let toHex =
        Array.map (fun (b: byte) -> b.ToString("x2")) >> (String.concat "")
    Encoding.ASCII.GetBytes >> md5.ComputeHash >> toHex

let avail ((x, y), path) =
    let hash = hash path
    let hopen i = 'b' <= hash.[i] &&  hash.[i] <= 'f'
    if x < 3 && hopen 3 then [("R", (1, 0))] else []
    |> (fun ds -> if x > 0 && hopen 2 then ("L", (-1, 0))::ds else ds)
    |> (fun ds -> if y < 3 && hopen 1 then ("D", (0, 1))::ds else ds)
    |> (fun ds -> if y > 0 && hopen 0 then ("U", (0, -1))::ds else ds)

let issol ((x, y), _) = match x, y with 3, 3 -> true | _ -> false

let expand ((x, y), path) =
    avail ((x, y), path)
    |> List.map (fun (dir, (x', y')) -> ((x + x', y + y'), path + dir))

let rec explore states = seq {
    match states with
    | [] -> ()
    | _ ->
        let sols, others = states |> (List.partition issol)
        yield! sols
        yield! others |> List.collect expand |> explore }

let info (passcode: string) (path: string) =
    (path.Substring(passcode.Length), path.Length - passcode.Length)

[<EntryPoint>]
let main argv =
    explore [origin]
    |> Seq.head
    |> (snd >> (info passcode) >> fst)
    |> printfn "Part 1: %A"

    explore [origin]
    |> Seq.last
    |> (snd >> (info passcode) >> snd)
    |> printfn "Part 2: %A"
    0
