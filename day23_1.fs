open System.IO

exception Failure of string

let _0 = Set.singleton 0

// let fileName = "/Users/jan/Downloads/input.t1"
let fileName = "/Users/jan/Downloads/input23.txt"
// let fileName = "/Users/jan/Downloads/input"

let lines = File.ReadAllLines fileName |> Seq.toList
// lines |> List.map (printfn "%s")

let parseLine (y: int, line: string) =
    let line = line.ToCharArray() |> Array.toList |> List.indexed
    line |> List.map (fun (x, c) -> (x, y), c)

type Pos = int * int

let map: Map<Pos, char> =
    lines
    |> List.rev
    |> List.indexed
    |> List.map parseLine
    |> List.concat
    |> Map.ofList

let maxX, maxY = map.Keys |> Seq.max

let start =
    map
    |> Map.toList
    |> List.filter (fun ((_, y), c) -> y = maxY && c = '.')
    |> List.head
    |> fst

let goal =
    map
    |> Map.toList
    |> List.filter (fun ((_, y), c) -> y = 0 && c = '.')
    |> List.head
    |> fst

printfn $"start = {start} goal = {goal}"

type Step = Pos * Pos * Pos list

let toS1 ((x, y): Pos) : string = $"{x}{y}"

let toSL (pl: Pos list) : string =
    pl |> List.map toS1 |> String.concat " "

let toS ((f, t, path): Step) : string =
    let path = path.Tail |> List.take (path.Length - 2)
    let path = path |> toSL
    $"[{f |> toS1}]{path}[{t |> toS1}]"

let allSteps: Step list =
    let walkable =
        map
        |> Map.toSeq
        |> Seq.filter (fun (_, c) -> c <> '#')
        |> Seq.map fst
        |> Set.ofSeq

    let pathsOfPoint ((x, y): Pos) =
        [ (x, y), (x + 1, y); (x, y), (x, y + 1) ]

    let paths: (Pos * Pos) list =
        walkable |> Set.toSeq |> Seq.map pathsOfPoint |> Seq.concat |> Seq.toList

    let paths = paths |> List.filter (fun (from, into) -> walkable.Contains into)
    let paths = paths |> List.map (fun (f, t) -> (f, t, [ f; t ]))
    paths

// allSteps |> List.map toS |> List.map (printfn "INITSTEP: %s")

let crossings: Set<Pos> =
    [ allSteps |> List.map (fun (p, _, _) -> p)
      allSteps |> List.map (fun (_, p, _) -> p) ]
    |> List.concat
    |> List.groupBy id
    |> List.filter (fun (_, l) -> l.Length > 2)
    |> List.map fst
    |> Set.ofList

crossings |> Set.map (printfn "Crossing: %A")

let stepEndsInCrossing ((f, t, _): Step) =
    crossings.Contains f || crossings.Contains t

let stepDoesntEndInCrossing s = stepEndsInCrossing s |> not


let rec shrinkFirst (remaining: Step list) (expanded: Step list) : Step list =
    match remaining with
    | [] -> expanded
    | p :: remaining when stepEndsInCrossing p ->
        let expanded = p :: expanded
        shrinkFirst remaining expanded
    | (from, into, path) :: remaining ->
        let before =
            remaining
            |> List.filter (fun (f, t, _) -> f = from || t = from)
            |> List.filter (fun (f, t, _) -> (crossings.Contains f || crossings.Contains t) |> not)

        let after =
            remaining
            |> List.filter (fun (f, t, _) -> f = into || t = into)
            |> List.filter (fun (f, t, _) -> (crossings.Contains f || crossings.Contains t) |> not)

        let expandable = before.Length = 1 || after.Length = 1

        if not expandable then
            let expanded = (from, into, path) :: expanded
            shrinkFirst remaining expanded
        else if before.Length = 1 then
            let (f, t, p) = before.Head
            let remaining = remaining |> List.filter (fun r -> r <> before.Head)
            let (f, t, p) = if f = from then (t, f, p |> List.rev) else (f, t, p)
            let path = [ p; path.Tail ] |> List.concat
            let curr = (f, into, path)
            shrinkFirst (curr :: remaining) expanded
        else
            let (f, t, p) = after.Head
            let remaining = remaining |> List.filter (fun r -> r <> after.Head)
            let (f, t, p) = if t = into then (t, f, p |> List.rev) else (f, t, p)
            let path = [ path; p.Tail ] |> List.concat
            let curr = (from, t, path)
            shrinkFirst (curr :: remaining) expanded

let steps = shrinkFirst allSteps []

let paths: Pos list list =
    steps
    |> List.map (fun (_, _, p) -> p)
    |> List.map (fun p -> [ p; p |> List.rev ])
    |> List.concat

let validStep (pos1: Pos) (pos2: Pos) : bool =
    let c1 = map[pos1]
    let c2 = map[pos2]

    match (pos1, pos2) with
    | _ when c1 = '.' && c2 = '.' -> true
    | (x1, _), (x2, _) when x2 = x1 - 1 ->
        // left
        let fromValid = c1 = '<' || c1 = '.'
        let toValid = c2 <> '>'
        fromValid && toValid
    | (x1, _), (x2, _) when x2 = x1 + 1 ->
        // right
        let fromValid = c1 = '>' || c1 = '.'
        let toValid = c2 <> '<'
        fromValid && toValid
    | (_, y1), (_, y2) when y2 = y1 - 1 ->
        // down
        let fromValid = c1 = 'v' || c1 = '.'
        let toValid = c2 <> '^'
        fromValid && toValid
    | (_, y1), (_, y2) when y2 = y1 + 1 ->
        // up
        let fromValid = c1 = '^' || c1 = '.'
        let toValid = c2 <> 'v'
        fromValid && toValid

let rec validPath (path: Pos list) =
    match path with
    | f :: t :: rest -> validStep f t && validPath (t :: rest)
    | _ -> true

type Edge = Pos * Pos * int
let edS (((x1, y1), (x2, y2), cost): Edge) : string = $"<{x1}{y1} {x2}{y2} {cost}>"

let edSL (edges: Edge list) =
    edges |> List.map edS |> String.concat ""

let edges: Edge list =
    paths
    |> List.filter validPath
    |> List.map (fun path -> path.Head, path |> List.last, (path.Length |> int) - 1)

let startToEdge = edges |> List.groupBy (fun (s, _, _) -> s) |> Map.ofList

let viableByContinueEnd (startToEdge: Map<Pos, Edge list>) ((s, t, _): Edge) : bool =
    if t = goal then
        true
    else
        let targets = startToEdge[t] |> List.filter (fun (_, t2, _) -> t2 <> s) // remove reversed

        if targets.IsEmpty then
            false
        elif targets.Length = 1 then
            printfn $"1 valid continuation: {(s, t)} -> {targets.Head}"
            true
        else
            true

let viableByEntry (endToEdge: Map<Pos, Edge list>) ((s, t, _): Edge) : bool =
    if s = start then
        true
    else
        let targets = endToEdge[s] |> List.filter (fun (s2, _, _) -> s2 <> t) // remove reversed

        if targets.IsEmpty then
            false
        elif targets.Length = 1 then
            printfn $"1 valid entry: {(s, t)} <- {targets.Head}"
            true
        else
            true

let notIntoStartOrFromEnd ((f, t, _): Edge) : bool = f <> goal && t <> start

let edges2 =
    let edges = edges |> List.filter notIntoStartOrFromEnd
    let edges = edges |> List.filter (viableByContinueEnd startToEdge)
    let startToEdge = edges |> List.groupBy (fun (s, _, _) -> s) |> Map.ofList
    let endToEdge = edges |> List.groupBy (fun (_, t, _) -> t) |> Map.ofList
    printfn $"2nd: {edges.Length}"
    let edges = edges |> List.filter (viableByContinueEnd startToEdge)
    let edges = edges |> List.filter (viableByEntry endToEdge)
    printfn $"3nd: {edges.Length}"
    edges |> List.map (printfn "%A")
    edges

let singleContinuationMap (startToEdge: Map<Pos, Edge list>) ((s, t, v): Edge) : (Edge * Edge) option =
    if t = goal then
        None
    else
        let targets = startToEdge[t] |> List.filter (fun (_, t2, _) -> t2 <> s) // remove reversed

        if targets.IsEmpty then
            None
        elif targets.Length = 1 then
            printfn $"1 valid continuation: {(s, t)} -> {targets.Head}"
            Some((s, t, v), targets.Head)
        else
            None

printfn "###"

let solve (edges:Edge list) =
    let edgesByFrom = edges |> List.groupBy (fun (f,_,_) -> f ) |> Map.ofList
    let edgesByDest = edges |> List.groupBy (fun (_,t,_) -> t ) |> Map.ofList

    let rec solve (curr:Pos) (visited:Set<Pos>) (steps:int) : int =
        printfn $"solve: {curr} {visited} {steps}"
        if curr = goal then 
            steps
        else
            let visited = visited.Add curr
            match edgesByFrom.TryFind curr with
            | None -> -1
            | Some(edges:Edge list) ->
                let edges = edges |> List.filter (fun (_,t,_) -> visited.Contains t |> not)
                printfn $"AT {curr} nexts = {edges}"
                if edges.IsEmpty then
                    printfn $"Dead end: {curr}"
                    -1
                else 
                    let followEdge ((f,t,c):Edge) : int =
                        let steps = steps + c
                        let curr = t
                        solve curr visited steps
                    edges |> List.map followEdge |> List.max
    solve start Set.empty 0

solve edges2 |> printfn "Solved: %A"       
