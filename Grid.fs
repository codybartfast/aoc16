module Grid
open System

let nl = Environment.NewLine

type Grid<'a>(data: 'a[][]) =

    let width = data.[0].Length
    let height = data.Length

    member _.Data with get() = data
    member _.Width with get() = width
    member _.Height with get() = height

    // accessors

    member _.Item
        with get(x, y) = data.[y].[x]
        and set(x, y) v = data.[y].[x] <- v

    member _.Row
        with get(y) = data.[y] |> Array.copy

    member _.Col
        with get(x) = data |> Array.map (fun arr -> arr.[x])

    // query

    member _.Count(pred) = Seq.concat data|> Seq.filter pred |> Seq.length

    // display

    member _.AsTextF<'a>(fmt) =
        String.concat nl (data |> Array.map (fun row ->
            String.concat "" (row |> Array.map fmt)))

    member this.AsText<'a>() =
        this.AsTextF(fun c -> c.ToString())

    override this.ToString() = this.AsText()

// =============================================================================
let fchar = function '.' -> " " | c -> c |> string

module Grid =

    // construction

    let init width height (gen: int -> int -> 'a) =
        [| for y in 0 .. (height - 1) do
            [| for x in 0 .. (width - 1) do
                gen x y |] |]
        |> Grid<'a>

    let initWith width height def = init width  height (fun _ _ -> def)

    // query

    let count pred (grid: Grid<'a>) = grid.Count pred

    // display

    let astext (grid: Grid<'a>) = grid.AsText()

    let astextf fmt (grid: Grid<'a>) = grid.AsTextF(fmt)
