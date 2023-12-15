open System.IO

exception Failure of string

let _0 = Set.singleton 0

let fileName = "/Users/jan/Downloads/input14"

let lines = File.ReadAllLines fileName |> Seq.toList

lines |> List.map (printfn "%A")

let parse (lines: string list) =
    let lines = lines |> List.rev |> List.indexed |> List.rev

    let parse ((y, line): int * string) =
        let line = line.ToCharArray() |> Array.toList
        let line = line |> List.indexed
        line |> List.map (fun (x, c) -> (x, y), c)

    lines |> List.map parse |> List.concat

type Pos = int * int

type Platform(maxXY: Pos, square: Set<Pos>, round: Set<Pos>) =
    let xs = seq { 0 .. (fst maxXY) } |> Seq.toList
    let ys = seq { 0 .. (snd maxXY) } |> Seq.toList
    let maxX = fst maxXY
    let maxY = snd maxXY

    override this.ToString() =
        $"Platform square:{square} round={round} [{maxXY}]"

    member this.Print() =
        let ys = seq { 0 .. (snd maxXY) } |> Seq.toList |> List.rev
        let xs = seq { 0 .. (fst maxXY) } |> Seq.toList

        let toLine (y: int) =
            let toChar (pos: Pos) =
                match (square.Contains pos), (round.Contains pos) with
                | (true, false) -> '#'
                | (false, true) -> 'O'
                | (true, true) -> 'X'
                | (false, false) -> '.'

            xs
            |> List.map (fun x -> x, y)
            |> List.map toChar
            |> List.toArray
            |> System.String.Concat

        ys |> List.map toLine |> List.map (printfn "%A")

    member this.TiltNorth() : Platform =
        let rec itery (x: int) (round: Set<Pos>) (free: Option<Pos>) y =
            if y < 0 then
                round
            else
                match (square.Contains(x, y), round.Contains(x, y)) with
                | (false, false) when free.IsNone -> itery x round (Some(x, y)) (y - 1)
                | (false, false) when free.IsSome -> itery x round free (y - 1)
                | (true, _) -> itery x round None (y - 1)
                | (_, true) when free.IsSome ->
                    let round = round.Remove(x, y)
                    let round = round.Add free.Value
                    itery x round None (snd free.Value)
                | (_, true) when free.IsNone -> itery x round None (y - 1)
                | (s, r) ->
                    printfn $"FAIL: {s} {r} {free}"
                    round

        let rec iterx (round: Set<Pos>) (x: int) =
            if x > maxX then
                round
            else
                let round = itery x round None maxY
                iterx round (x + 1)

        let round = iterx round 0
        Platform(maxXY, square, round)

    member this.TiltSouth() : Platform =
        let rec itery (x: int) (round: Set<Pos>) (free: Option<Pos>) y =
            if y > maxY then
                round
            else
                match (square.Contains(x, y), round.Contains(x, y)) with
                | (false, false) when free.IsNone -> itery x round (Some(x, y)) (y + 1)
                | (false, false) when free.IsSome -> itery x round free (y + 1)
                | (true, _) -> itery x round None (y + 1)
                | (_, true) when free.IsSome ->
                    let round = round.Remove(x, y)
                    let round = round.Add free.Value
                    itery x round None (snd free.Value)
                | (_, true) when free.IsNone -> itery x round None (y + 1)
                | (s, r) ->
                    printfn $"FAIL: {s} {r} {free}"
                    round

        let rec iterx (round: Set<Pos>) (x: int) =
            if x > maxX then
                round
            else
                let round = itery x round None 0
                iterx round (x + 1)

        let round = iterx round 0
        Platform(maxXY, square, round)

    member this.TiltWest() : Platform =
        let rec iterx (y: int) (round: Set<Pos>) (free: Option<Pos>) x =
            if x > maxX then
                round
            else
                match (square.Contains(x, y), round.Contains(x, y)) with
                | (false, false) when free.IsNone -> iterx y round (Some(x, y)) (x + 1)
                | (false, false) when free.IsSome -> iterx y round free (x + 1)
                | (true, _) -> iterx y round None (x + 1)
                | (_, true) when free.IsSome ->
                    let round = round.Remove(x, y)
                    let round = round.Add free.Value
                    iterx y round None (fst free.Value)
                | (_, true) when free.IsNone -> iterx y round None (x + 1)
                | (s, r) ->
                    printfn $"FAIL: {s} {r} {free}"
                    round

        let rec itery (round: Set<Pos>) (y: int) =
            if y > maxY then
                round
            else
                let round = iterx y round None 0
                itery round (y + 1)

        let round = itery round 0
        Platform(maxXY, square, round)
        
    member this.TiltEast() : Platform =
        let rec iterx (y: int) (round: Set<Pos>) (free: Option<Pos>) x =
            if x < 0 then
                round
            else
                match (square.Contains(x, y), round.Contains(x, y)) with
                | (false, false) when free.IsNone -> iterx y round (Some(x, y)) (x - 1)
                | (false, false) when free.IsSome -> iterx y round free (x - 1)
                | (true, _) -> iterx y round None (x - 1)
                | (_, true) when free.IsSome ->
                    let round = round.Remove(x, y)
                    let round = round.Add free.Value
                    iterx y round None (fst free.Value)
                | (_, true) when free.IsNone -> iterx y round None (x - 1)
                | (s, r) ->
                    printfn $"FAIL: {s} {r} {free}"
                    round

        let rec itery (round: Set<Pos>) (y: int) =
            if y > maxY then
                round
            else
                let round = iterx y round None maxX
                itery round (y + 1)

        let round = itery round 0
        Platform(maxXY, square, round)
        
    member this.Score() =
        round |> Set.toList |> List.map snd |> List.map (fun y -> y + 1) |> List.sum

    member this.CacheKey = round 

let toPlatform (input: (Pos * char) list) =
    let maxXY = input |> List.map fst |> List.max

    let square =
        input |> List.filter (fun (_, c) -> c = '#') |> List.map fst |> Set.ofList

    let round =
        input |> List.filter (fun (_, c) -> c = 'O') |> List.map fst |> Set.ofList

    Platform(maxXY, square, round)

let platform = parse lines |> toPlatform
printfn ""
platform.Print()
printfn ""
let platform1 = platform.TiltNorth()
platform1.Print()
let score = platform1.Score()

printfn $"SCORE: {score}"

printfn ""
let platform2 = platform.TiltSouth()
platform2.Print()

printfn ""

let platform3 = platform.TiltWest()
platform3.Print()

printfn ""
let platform4 = platform.TiltEast()
platform4.Print()

let cycle (platform:Platform) : Platform =
    let platform = platform.TiltNorth()
    let platform = platform.TiltWest()
    let platform = platform.TiltSouth()
    let platform = platform.TiltEast()
    platform 
    
let rec cycleN (n:int) (platform:Platform) =
    if n = 0 then platform else 
    let platform = cycle platform
    printfn $"{n}: {platform.Score()}"
    // platform.Print()
    cycleN (n-1) platform
    
// cycleN 1000 platform

let rec findLoop (n:int) (cache: Set<Set<Pos>>) (platform:Platform) =
    printfn $"findLoop {n}"
    if cache.Contains platform.CacheKey then
        n,platform 
    elif n > 1000 then
        raise (Failure $"GIVING UP AT {n}")
    else
        let cache = cache.Add platform.CacheKey
        let platform = cycle platform 
        findLoop (n+1) cache platform
        
let solve (platform:Platform) =
    let loopStart,platform = findLoop 0 Set.empty platform
    printfn $"loopstart {loopStart} {platform}"
    let cache = Set.singleton platform.CacheKey
    let platform = cycle platform
    let loopSize,platform = findLoop 1 cache platform
    printfn $"loopSize {loopSize} {platform}"
    let loopStart = loopStart |> int64
    let loopSize = loopSize |> int64
    let ALL_ITERS = 1_000_000_000L
    let rem = (ALL_ITERS - loopStart) % loopSize
    printfn $"REM {rem}"
    let platform = cycleN (rem |> int) platform
    printfn $"SCORE = {platform.Score}"
        
solve platform 