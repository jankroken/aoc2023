open System.IO

exception Failure of string

let _0 = Set.singleton 0

let fileName = "/Users/jan/Downloads/input"

let lines = File.ReadAllLines fileName |> Seq.toList
lines |> List.map (printfn "%s")

type Pos = int64 * int64

let parse (lines: string list) =
    let lines = lines |> List.rev |> List.indexed

    let parse ((y, line): int * string) =
        let line = line.ToCharArray() |> Array.toList |> List.indexed
        line |> List.map (fun (x, c) -> ((x |> int64, y |> int64), c))

    lines |> List.map parse |> List.concat |> Map.ofList

let farm = lines |> parse

let guardenSpots =
    farm
    |> Map.toList
    |> List.filter (fun (_, c) -> c = '.' || c = 'S')
    |> List.map (fun (pos, _) -> pos)
    |> Set.ofList

let startSpot: Pos =
    farm |> Map.toList |> List.filter (fun (_, c) -> c = 'S') |> List.head |> fst

farm |> printfn "%A"

let rec addAllToMap (map: Map<Pos, int64>) (n: int64) (poss: Pos list) =
    match poss with
    | [] -> map
    | pos :: poss ->
        let map = map.Add(pos, n)
        addAllToMap map n poss

let rec waterflow
    (n: int64)
    (wet: Set<Pos>)
    (reachable: Set<Pos>)
    (reachableSteps: Map<Pos, int64>)
    (guardenSpots: Set<Pos>)
    =
    if wet.IsEmpty then
        reachable, reachableSteps
    else
        let guardenSpots = Set.difference guardenSpots wet
        let reachable = Set.union reachable wet
        let reachableSteps = addAllToMap reachableSteps n (wet |> Set.toList)

        let neighbours =
            wet
            |> Set.toList
            |> List.map (fun ((x, y): Pos) -> [ (x + 1L, y); (x - 1L, y); (x, y + 1L); (x, y - 1L) ])
            |> List.concat
            |> Set.ofList

        let wet = Set.intersect neighbours guardenSpots
        waterflow (n + 1L) wet reachable reachableSteps guardenSpots

let reachableSpots, reachableSteps =
    waterflow 0 (Set.singleton startSpot) Set.empty Map.empty guardenSpots

reachableSteps |> (printfn "%A")

let findInSteps (n: int64) (reachableSteps: Map<Pos, int64>) =
    reachableSteps
    |> Map.toList
    |> List.map snd
    |> List.filter (fun steps -> steps <= n)
    |> List.filter (fun steps -> steps % 2L = n % 2L)
    |> List.length

let answerFor6 = findInSteps 6L reachableSteps

printfn $"answer for 6 = {answerFor6}"

let answerFor64 = findInSteps 64L reachableSteps

printfn $"answer for 64 = {answerFor64}"

let allSpots = reachableSpots.Count
let width = lines.Head.Length |> int64
let height = lines.Length |> int64


let numDiags (steps: int64) (baseSteps: int64) =
    let available = steps - baseSteps

    if available < 0L then
        0L
    elif available = 0L then
        1L 
    else
        let maxX = available / width

        let availableY =
            seq { 0L .. maxX }
            |> Seq.map (fun n -> n * width)
            |> Seq.map (fun n -> available - n)
            |> Seq.toList

        let availableY = availableY |> List.map (fun steps -> steps / available + 1L)
        (availableY |> List.sum)
       
let solveDiagsPart2 (steps: int64) =
    let initNW = reachableSteps[(0L, height - 1L)]
    let initSW = reachableSteps[(0L, 0L)]
    let initNE = reachableSteps[width - 1L, height - 1L]
    let initSE = reachableSteps[width - 1L, 0L]
    printfn $"initSW = {initSW} initNW = {initNW} initSE={initSE} initNE={initNE}"

    let _, reachableFromSW =
        waterflow 2 (Set.singleton (0L, 0L)) Set.empty Map.empty guardenSpots

    let _, reachableFromSE =
        waterflow 2 (Set.singleton (width-1L, 0L)) Set.empty Map.empty guardenSpots

    let _, reachableFromNW =
        waterflow 2 (Set.singleton (0, height-1L)) Set.empty Map.empty guardenSpots

    let _, reachableFromNE =
        waterflow 2 (Set.singleton (width-1L, height-1L)) Set.empty Map.empty guardenSpots

    let baseSW =
        reachableFromSW |> Map.values |> Seq.toList |> List.map (fun v -> v + initSW)

    let baseNW =
        reachableFromNW |> Map.values |> Seq.toList |> List.map (fun v -> v + initNW)

    let baseSE =
        reachableFromSE |> Map.values |> Seq.toList |> List.map (fun v -> v + initSE)

    let baseNE =
        reachableFromNE |> Map.values |> Seq.toList |> List.map (fun v -> v + initNE)

    let baseDiags = [ baseSW; baseNW; baseSE; baseNE ] |> List.concat
    printfn $"width = {width} height={height}"
    let diags = baseDiags  |> List.map (numDiags steps)
    printfn $"diags {diags}"
    diags |> List.sum 

let solveUp (steps:int64) =
    let hasBetterNeighbour (((x,y),steps):Pos*int64) =
        let l = reachableSteps.TryFind(x-1L,y)
        let r = reachableSteps.TryFind(x+1L,y)
        match (l,r) with
        | Some(i),_ when i < steps -> true
        | _,Some(i) when i < steps -> true
        | _ -> false
                                       
    let top1 =
        reachableSteps |> Map.toList
        |> List.filter (fun ((x,y),_ ) -> y = (height - 1L))
        |> List.filter (fun pd -> hasBetterNeighbour pd |> not)
        
    printfn $"{steps} {top1}"


let solvePart2 (steps:int64) =
    let startMapCount = findInSteps steps reachableSteps
    let diags = solveDiagsPart2 steps
    printfn $"start={startMapCount} DIAGS: {diags}"


solvePart2 6L

solveUp 6L
