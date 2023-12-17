open System.IO

exception Failure of string

let _0 = Set.singleton 0

let fileName = "/Users/jan/Downloads/input16"

let lines = File.ReadAllLines fileName |> Seq.toList
lines |> List.map (printfn "%A")

type Pos = int * int

let parse (lines: string list) : Map<Pos, char> =
    let parse ((y, line): int * string) =
        line.ToCharArray()
        |> Array.toList
        |> List.indexed
        |> List.map (fun (x, c) -> (x, y), c)

    lines |> List.rev |> List.indexed |> List.map parse |> List.concat |> Map.ofList

type Dir =
    | N
    | S
    | E
    | W

type Beam = Pos * Dir

type Cave(map: Map<Pos, char>) =
    let (maxX, maxY) = map.Keys |> Seq.max
    let xs = seq { 0..maxX } |> Seq.toList
    let ys = seq { 0..maxY } |> Seq.toList

    member this.Map = map

    member this.Move(beam: Beam) : Beam list =
        if map.ContainsKey(fst beam) |> not then
            []
        else
            let c = map[fst beam]
            let (x, y) = fst beam
            let dir = snd beam

            match c, dir with
            | '.', E -> [ (x + 1, y), E ]
            | '|', E -> [ (x, y + 1), N; (x, y - 1), S ]
            | '-', E -> [ (x + 1, y), E ]
            | '/', E -> [ (x, y + 1), N ]
            | '\\', E -> [ (x, y - 1), S ]
            | '.', W -> [ (x - 1, y), W ]
            | '|', W -> [ (x, y + 1), N; (x, y - 1), S ]
            | '-', W -> [ (x - 1, y), W ]
            | '/', W -> [ (x, y - 1), S ]
            | '\\', W -> [ (x, y + 1), N ]
            | '.', N -> [ (x, y + 1), N ]
            | '|', N -> [ (x, y + 1), N ]
            | '-', N -> [ (x - 1, y), W; (x + 1, y), E ]
            | '/', N -> [ (x + 1, y), E ]
            | '\\', N -> [ (x - 1, y), W ]
            | '.', S -> [ (x, y - 1), S ]
            | '|', S -> [ (x, y - 1), S ]
            | '-', S -> [ (x - 1, y), W; (x + 1, y), E ]
            | '/', S -> [ (x - 1, y), W ]
            | '\\', S -> [ (x + 1, y), E ]

    override this.ToString() = $"Cave[{maxX},{maxY}]"

    member this.Print() =
        printfn "Cave:"
        let ys = ys |> List.rev

        let printLine y =
            xs
            |> List.map (fun x -> (x, y))
            |> List.map (fun m -> map[m])
            |> List.toArray
            |> System.String.Concat
            |> (fun l -> printfn $"  {l}")

        ys |> List.map printLine

    member this.StartBeam = Beam((0, maxY), E)
    
    member this.AllStartBeams() : Beam list =
        [xs |> List.map (fun x -> (x,0),N);
         xs |> List.map (fun x -> (x,maxY),S);
         ys |> List.map (fun y -> (0,y),E)
         ys |> List.map (fun y -> (maxX,y),W)]
        |> List.concat 
let cave = Cave(parse lines)
cave.Print()

let (maxX, maxY) = cave.Map.Keys |> Seq.max
let xs = seq { 0..maxX } |> Seq.toList
let ys = seq { 0..maxY } |> Seq.toList

type EnergyMap(map: Map<Pos, bool>) =

    member this.Map = map

    member this.Energize(beam: Beam) =
        if map.ContainsKey(fst beam) |> not then
            this
        else
            let pos = fst beam
            let map = map.Add(pos, true)
            EnergyMap(map)
    
    member this.EnergizedCount() =
        map.Values |> Seq.filter id |> Seq.length

    override this.ToString() = $"EnergyMap[{maxX},{maxY}]"

    member this.Print() =
        printfn "Cave:"
        let ys = ys |> List.rev

        let printLine y =
            xs
            |> List.map (fun x -> (x, y))
            |> List.map (fun m -> map[m])
            |> List.map (fun b -> if b then '#' else '.')
            |> List.toArray
            |> System.String.Concat
            |> (fun l -> printfn $"  {l}")

        ys |> List.map printLine

let toEmptyEnergyMap (cave: Cave) : EnergyMap =
    cave.Map
    |> Map.toList
    |> List.map (fun (p, c) -> (p, false))
    |> Map.ofList
    |> EnergyMap


let noEnergy = toEmptyEnergyMap cave

let solve (cave: Cave) (beam:Beam): Set<Beam> * EnergyMap =

    let rec solve (cache: Set<Beam>) (energy: EnergyMap) (beams: Beam list) : Set<Beam> * EnergyMap =
        if beams.IsEmpty then
            cache, energy
        else
            let beam = beams.Head
            let beams = beams.Tail

            if cache.Contains beam then
                solve cache energy beams
            else
                let stopWatch = System.Diagnostics.Stopwatch.StartNew()
                let energy = energy.Energize beam
                stopWatch.Stop()
                let t1 = stopWatch.Elapsed.TotalMilliseconds 
                let stopWatch = System.Diagnostics.Stopwatch.StartNew()
                let cache = cache.Add beam
                stopWatch.Stop()
                let t2 = stopWatch.Elapsed.TotalMilliseconds
                let stopWatch = System.Diagnostics.Stopwatch.StartNew()
                let beams =
                    match cave.Move beam with
                        | [] -> beams
                        | [a] -> a::beams
                        | [a;b] -> a::b::beams
                stopWatch.Stop()
                let t3 = stopWatch.Elapsed.TotalMilliseconds
                printfn $"TIME123: {t1} {t2} {t3}"
                solve cache energy beams

    solve Set.empty noEnergy [ beam ]

let stopWatch = System.Diagnostics.Stopwatch.StartNew()
let _, energyMap = solve cave cave.StartBeam
stopWatch.Stop()

energyMap.Print()

printfn $"ANSWER 1 = {energyMap.EnergizedCount()}"

printfn $"TIME: {stopWatch.Elapsed.TotalMilliseconds}"

printfn $"AllStartBeams: {cave.AllStartBeams().Length}"


let answer2 =
    let beams = cave.AllStartBeams()
    let solve(beam:Beam) : int =
        let _,e = solve cave beam
        e.EnergizedCount()
    beams |> List.map solve |> List.max

printfn $"ANSWER 2: {answer2}"

