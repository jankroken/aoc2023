open System.IO

exception Failure of string

let _0 = Set.singleton 0

let fileName = "/Users/jan/Downloads/input12"

let lines = File.ReadAllLines fileName |> Seq.toList

let toS (s: Set<int>) =
    s |> Set.toList |> List.map (fun f -> $"{f}") |> String.concat ""

let toSS (s: string list) = s |> String.concat " "

let toCS (s: char list) =
    s |> List.toArray |> System.String.Concat

let toLS (ls: Set<int> list) = ls |> List.map toS |> String.concat " "

let toL (l: _ list) =
    l |> List.map (fun i -> $"{i}") |> String.concat (" ")

let toLP (l: _ list) =
    l |> List.map (fun i -> $"{i}") |> String.concat ("|")

type Strip(strip: char list, groups: int list) =

    member this.Strip = strip
    member this.Groups = groups

    member this.MemoKey = strip.Length,groups.Length 
    override this.ToString() =
        $"Strip {strip |> toCS} : {groups |> toL}"

    member this.Place() =
        if groups.Length = 0 then
            None
        else
            let n: int = groups.Head

            if strip.Length < n then
                None
            elif strip |> List.take n |> List.contains ('.') then
                None
            else
                let strip = strip |> List.skip n

                if strip.IsEmpty then Some(Strip([], groups.Tail))
                elif strip.Head = '#' then None
                else Some(Strip(strip.Tail, groups.Tail))

    member this.Skip() =
        if strip.IsEmpty then None
        elif strip.Head = '#' then None
        else Some(Strip(strip.Tail, groups))

    member this.Finished = strip.IsEmpty

    member this.Score() : int64 =
        if strip.IsEmpty && groups.IsEmpty then 1L else 0L

let parse (line: string) =
    let line = line.Split(" ")
    let strip = line[0].ToCharArray() |> Array.toList
    let groups = line[1].Split(",") |> Array.map (int) |> Array.toList
    Strip(strip, groups)
    
let parse2 (line: string) =
    let line = line.Split(" ")
    let strip = line[0] |> List.replicate 5 |> String.concat "?"
    let strip = strip.ToCharArray() |> Array.toList
    let groups = line[1] |> List.replicate 5 |> String.concat ","
    let groups = groups.Split(",") |> Array.map (int) |> Array.toList
    Strip(strip, groups)

let strips = lines |> List.map parse
let strips2 = lines |> List.map parse2

// strips |> List.map (fun s -> printfn $"{s}     {s.Place()}      {s.Skip()}")

type Memo = Map<int * int, int64>

let rec solve (memo: Map<int * int, int64>) (strip: Strip) : Memo * int64 =
    // printfn $"solve {memo} {strip}"

    if strip.Finished then
        memo, strip.Score()
    elif memo.ContainsKey strip.MemoKey then
        // printfn $"memoized"
        memo, memo[strip.MemoKey]
    else
        let memo, score1 =
            strip.Place() |> Option.map (solve memo) |> Option.defaultValue (memo, 0)

        let memo, score2 =
            strip.Skip() |> Option.map (solve memo) |> Option.defaultValue (memo, 0)
        let memo = memo.Add (strip.MemoKey, score1+score2)
        // printfn $"solve(2) {memo}"
        memo, score1 + score2

let _, res = solve Map.empty strips.Head

printfn $"res = {res}"

let stopWatch = System.Diagnostics.Stopwatch.StartNew()
let res2 = strips2 |> List.map (solve Map.empty)
stopWatch.Stop()

res2 |> List.map snd |> List.map (printfn "%A")

res2 |> List.map snd |> List.sum |> printfn "ANSWER %A"

printfn $"TIME: {stopWatch.Elapsed.TotalMilliseconds}"

// strips |> List.map (printfn "%A")

