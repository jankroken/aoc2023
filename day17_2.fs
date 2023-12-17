open System.IO

exception Failure of string

let _0 = Set.singleton 0

let fileName = "/Users/jan/Downloads/input17"

let lines = File.ReadAllLines fileName |> Seq.toList
lines |> List.map (printfn "%A")

let parse (lines: string list) =
    let lines = lines |> List.rev |> List.indexed

    let parse ((y, line): int * string) =
        let toInt (c: char) = c - '0' |> int
        let line = line.ToCharArray() |> Array.map toInt |> Array.toList |> List.indexed
        line |> List.map (fun (x, i) -> (x, y), i)

    lines |> List.map parse |> List.concat |> Map.ofList

let block = lines |> parse
let (maxX, maxY) = block.Keys |> Seq.max
let xs = seq { 0..maxX } |> Seq.toList
let ys = seq { 0..maxY } |> Seq.toList
type Pos = int * int
let start: Pos = (0, maxY)
let dest: Pos = (maxX, 0)

type Direction =
    | N
    | S
    | E
    | W

let turnLeft (dir: Direction) =
    match dir with
    | N -> W
    | W -> S
    | S -> E
    | E -> N

let turnRight (dir: Direction) =
    match dir with
    | N -> E
    | E -> S
    | S -> W
    | W -> N

let step ((x, y): Pos) (dir: Direction) : Pos =
    match dir with
    | N -> (x, y + 1)
    | S -> (x, y - 1)
    | W -> (x - 1, y)
    | E -> (x + 1, y)

let distanceToDest ((x, y): Pos) =
    let (dx, dy) = dest
    (dx + dy) - (x + y)

let valid ((x, y): Pos) =
    x >= 0 && x <= maxX && y >= 0 && y <= maxY

type CarKey = Pos * Direction * int

type Car(pos: Pos, dir: Direction, steps: int, loss: int) =
    member this.Pos = pos
    member this.Dir = dir
    member this.Steps = steps
    member this.Loss = loss

    member this.IsValid = steps < 11 && steps > -1 && valid pos

    member this.Nexts() : Car list =
        let getLoss pos =
            if block.ContainsKey pos then loss + block[pos] else -1

        let left = turnLeft dir
        let left = Car(step pos left, left, (if steps > 3 then 1 else -1), step pos left |> getLoss)
        let right = turnRight dir
        let right = Car(step pos right, right, (if steps > 3 then 1 else -1), step pos right |> getLoss)
        let fwd = Car(step pos dir, dir, steps + 1, step pos dir |> getLoss)
        let nexts = [ left; right; fwd ] |> List.filter (_.IsValid)
        nexts

    member this.Key: CarKey = pos, dir, steps

    member this.BetterKeys: CarKey list =
        seq { 4..9 }
        |> Seq.filter (fun n -> n < steps)
        |> Seq.map (fun steps -> (pos, dir, steps))
        |> Seq.toList

    override this.ToString() =
        $"Car {pos} {dir} steps: {steps} loss: {loss}"

type CostMap = Map<CarKey, Car>

let isBetter (costs: CostMap) (car: Car) =
    if costs.ContainsKey car.Key then
        costs[car.Key].Loss > car.Loss
    else
        true

let rec addCars (costs: CostMap) (cars: Car list) =
    match cars with
    | [] -> costs
    | car :: cars ->
        let costs = costs.Add(car.Key, car)
        addCars costs cars

let uniqueCars (cars: Car list) =
    let cheapest (cars: Car list) = cars |> List.minBy _.Loss
    cars |> List.groupBy (_.Key) |> List.map snd |> List.map cheapest

let solve () =
    let costs: CostMap = Map.empty
    let start1 = Car(start, S, 0, 0)
    let start2 = Car(start, E, 0, 0)
    let costs = costs.Add(start1.Key, start1)
    let costs = costs.Add(start2.Key, start2)
    printfn $"costs: {costs}"

    let rec solve (costs: CostMap) (fresh: Car list) =
        if fresh.Length = 0 then
            costs
        else
            printfn $"solve: {fresh.Length}"
            let nexts = fresh |> List.map (fun c -> c.Nexts()) |> List.concat
            let better = nexts |> List.filter (isBetter costs) |> uniqueCars
            let costs = addCars costs better
            solve costs better 

    solve costs [ start1; start2 ]

let costs = solve ()

let finalCars : Car list =
    costs |> Map.toList |> List.map snd |> List.filter (fun c -> c.Pos = dest)
    |> List.filter (fun car -> car.Steps > 3)

printfn $"finalCars: {finalCars |> List.map _.Loss} {finalCars}"
let answer = finalCars |> List.map _.Loss |> List.min

printfn $"Answer = {answer}"