open System.IO

exception Failure of string

let _0 = Set.singleton 0

let fileName = "/Users/jan/Downloads/input18"

let lines = File.ReadAllLines fileName |> Seq.toList
// lines |> List.map (printfn "%s")

type Dir =
    | L
    | R
    | U
    | D

type Color = string
type Pos = int64 * int64

type Instruction(dir: Dir, steps: int64, color: Color) =
    member this.Dir = dir
    member this.Steps = steps
    member this.Color = color
    override this.ToString() = $"({dir} {steps} {color}"

let rec toHex (curr: int64) (s: char list) : int64 =
    match s with
    | [] -> curr
    | c :: rest when c >= '0' && c <= '9' ->
        let curr = curr * 16L
        let curr = curr + ((c - '0') |> int64)
        toHex curr rest
    | '#' :: rest -> toHex curr rest
    | c :: rest when c >= 'a' && c <= 'f' ->
        let curr = curr * 16L
        let curr = curr + ((c - 'a') |> int64) + 10L
        toHex curr rest

let parse (line: string) =
    let line = line.Split(" ")[2]
    let line = line.ToCharArray() |> Array.toList
    let line = line.Tail
    let line = line |> List.take (line.Length-1)

    let dir =
        match line |> List.last with
        | '0' -> R
        | '1' -> D
        | '2' -> L
        | '3' -> U

    let line = line |> List.take (line.Length-1)
    let steps = line |> toHex 0L
     
    let color = "BANANA"
    Instruction(dir, steps, color)

let instructions = lines |> List.map parse

instructions |> List.map (printfn "%A")


type Holes = Map<Pos, Color>

type Square = Pos * Pos

let rec dig ((x, y): Pos) (instructions: Instruction list) : Square list =
    match instructions with
    | [] -> []
    | inst :: instructions ->
        match inst.Dir with
        | L -> ((x - inst.Steps, y), (x, y)) :: (dig (x - inst.Steps, y) instructions)
        | R -> ((x, y), (x + inst.Steps, y)) :: dig (x + inst.Steps, y) instructions
        | U -> ((x, y), (x, y + inst.Steps)) :: dig (x, y + inst.Steps) instructions
        | D -> ((x, y - inst.Steps), (x, y)) :: dig (x, y - inst.Steps) instructions

// bordered |> Map.toList |> List.map (printfn "%A")

let trenches = dig (0, 0) instructions

trenches |> List.map (printfn "%A")

let xseps: int64 list =
    trenches
    |> List.map (fun ((x, _), (x2, _)) -> [ x; x2 ])
    |> List.concat
    |> List.distinct
    |> List.sort

let yseps: int64 list =
    trenches
    |> List.map (fun ((_, y), (_, y2)) -> [ y; y2 ])
    |> List.concat
    |> List.distinct
    |> List.sort

let splitTrench (((x1, y1), (x2, y2)): Square) : Square list =
    let xseps = xseps |> List.filter (fun x -> x >= x1 && x <= x2)
    let yseps = yseps |> List.filter (fun y -> y >= y1 && y <= y2)

    if xseps.Length > 0 && x1 < x2 then
        let rec split (x1, x2) (xs: int64 list) =
            // printfn $"split {(x1,x2)} xs:{xs}"
            match xs with
            | [] -> [ (x1, x2) ]
            | xs :: rest when xs > x1 && xs < x2 -> (x1, xs - 1L) :: (xs, xs) :: split (xs + 1L, x2) rest
            | xs :: rest when xs > x1 && xs = x2 -> [ (x1, xs - 1L); (xs, xs) ]
            | xs :: rest when xs = x1 && xs < x2 -> (xs, xs) :: split (xs + 1L, x2) rest
            | xs :: rest when xs = x1 && xs = x2 -> [ (xs, xs) ]

        let split = split (x1, x2) xseps
        // printf $"SPLIT-> "
        // split |> List.map (printf "%A")
        // printfn ""
        split |> List.map (fun (x1, x2) -> ((x1, y1), (x2, y2)))
    elif yseps.Length > 0 && y1 < y2 then
        let rec split (y1, y2) (ys: int64 list) =
            printfn $"split {(y1, y2)} ys:{ys}"

            match ys with
            | [] -> [ (y1, y2) ]
            | ys :: rest when ys > y1 && ys < y2 ->
                printfn $"YS 1 {ys} {y1} {y2}"
                (y1, ys - 1L) :: (ys, ys) :: split (ys + 1L, y2) rest
            | ys :: rest when ys > y1 && ys = y2 ->
                printfn $"YS 2 {ys} {y1} {y2}"
                [ (y1, ys - 1L); (ys, ys) ]
            | ys :: rest when ys = y1 && ys < y2 ->
                printfn $"YS 3 {ys} {y1} {y2}"
                (ys, ys) :: split (ys + 1L, y2) rest
            | ys :: rest when ys = y1 && ys = y2 -> [ (ys, ys) ]

        let split = split (y1, y2) yseps
        printf $"SPLIT-> "
        split |> List.map (printf "%A")
        printfn ""

        split |> List.map (fun (y1, y2) -> ((x1, y1), (x2, y2)))
    else
        [ (x1, y1), (x2, y2) ]

let holes =
    trenches |> List.map splitTrench |> List.concat |> List.sort |> Set.ofList

let areas: Square list =
    let xseps =
        [ [ xseps.Head - 1L ]; xseps; [ (xseps |> List.last) + 1L ] ] |> List.concat

    let yseps =
        [ [ yseps.Head - 1L ]; yseps; [ (yseps |> List.last) + 1L ] ] |> List.concat

    let rec segments (seps: int64 list) : (int64 * int64) list =
        match seps with
        | [] -> []
        | [ a ] -> [ (a, a) ]
        | a :: b :: rest when a + 1L = b -> (a, a) :: segments (b :: rest)
        | a :: b :: rest -> (a, a) :: (a + 1L, b - 1L) :: segments (b :: rest)

    let xsegs = segments xseps
    let ysegs = segments yseps

    xsegs
    |> List.map (fun (x1, x2) -> ysegs |> List.map (fun (y1, y2) -> (x1, y1), (x2, y2)))
    |> List.concat
    |> List.distinct

let areaHoles = areas |> List.filter (holes.Contains)
printfn $"{areaHoles.Length} = {holes.Count}"

areas |> List.map (printfn "%A")

Set.difference holes (areaHoles |> Set.ofList)
|> Set.map (printfn "NONMATCH: %A")

let untouchedAreas = Set.difference (areas |> Set.ofList) holes

let untouchedStart = untouchedAreas.MinimumElement

printfn $"Start: {untouchedStart}"

let rec waterflow (areas: Set<Square>) (wet: List<Square>) =
    let wet = wet |> List.filter areas.Contains

    if wet.IsEmpty then
        areas
    else
        let areas = Set.difference areas (wet |> Set.ofList)

        let isNeighbour (((x1, y1), (x2, y2)): Square) (((ox1, oy1), (ox2, oy2)): Square) =
            let left = y1 = oy1 && y2 = oy2 && x1 = ox2 + 1L
            let right = y1 = oy1 && y2 = oy2 && x2 + 1L = ox1
            let above = x1 = ox1 && x2 = ox2 && y2 + 1L = oy1
            let below = x1 = ox1 && x2 = ox2 && y1 = oy2 + 1L
            left || right || above || below

        let neighbors =
            wet
            |> List.map (fun wet -> areas |> Set.filter (isNeighbour wet))
            |> Set.unionMany
            |> Set.toList

        printfn $"Neighbours: {neighbors}"
        waterflow areas neighbors

let internals = waterflow untouchedAreas [ untouchedStart ]

let area (((x1, y1), (x2, y2)): Square) =
    let x = x2 - x1 + 1L
    let y = y2 - y1 + 1L
    x * y

let innerArea = internals |> Set.toList |> List.map area |> List.sum
let holeArea = holes |> Set.toList |> List.map area |> List.sum

printfn $"INNER {innerArea} + {holeArea} = {innerArea + holeArea}"



toHex 0L ("#70c71".ToCharArray() |> Array.toList) |> printfn "TOHEX %A"

toHex 0L ['#';'7';'1';'a'] |> printfn "1ax = {%A}"