open System.IO
open System.Numerics
open System.Security

exception Failure of string

let _0 = Set.singleton 0

let fileName = "/Users/jan/Downloads/input12.txt"

let lines = File.ReadAllLines fileName |> Seq.toList

let toS (s: Set<int>) =
    s |> Set.toList |> List.map (fun f -> $"{f}") |> String.concat ""

let toSS (s: string list) = s |> String.concat " "

let toLS (ls: Set<int> list) = ls |> List.map toS |> String.concat " "

let toL (l: _ list) =
    l |> List.map (fun i -> $"{i}") |> String.concat (" ")

let toLP (l: _ list) =
    l |> List.map (fun i -> $"{i}") |> String.concat ("|")

// lines |> List.map (printfn "%A")

type Surr(i: int, map: Map<int, int list>) =
    let i = i

    let g =
        map
        |> Map.toList
        |> List.filter (fun (_, b) -> b |> List.contains i)
        |> List.head
        |> fst

    let line = map[g]
    let index = line |> List.findIndex (fun e -> e = i)
    let directBefore = if index = 0 then 0 else line[index - 1]

    let directAfter = if index = line.Length - 1 then 0 else line[index + 1]

    let allBefore =
        let ing = line |> List.takeWhile (fun c -> c <> i) |> Set.ofList
        let gs = map |> Map.toList |> List.filter (fun (gn, _) -> gn < g)
        let gs = gs |> List.map snd |> List.concat |> Set.ofList
        Set.union ing gs |> Set.remove directBefore

    let allAfter =
        let ing = line |> List.skip (index + 1) |> Set.ofList
        let gs = map |> Map.toList |> List.filter (fun (gn, _) -> gn > g)
        let gs = gs |> List.map snd |> List.concat |> Set.ofList
        Set.union ing gs |> Set.remove directAfter

    member this.I = i
    member this.DirectBefore = directBefore
    member this.DirectAfter = directAfter
    member this.AllBefore = allBefore
    member this.AllAfter = allAfter

    override this.ToString() =
        let allBefore = allBefore |> toS
        let allAfter = allAfter |> toS

        $"({allBefore}<{directBefore}[{i}]{directAfter}>{allAfter})"

type Record(record: char list, check: int list) =
    let sizes = check |> List.indexed |> Map.ofList
    let keys = sizes.Keys |> Set.ofSeq
    let record = record

    let groups =
        let rec groups (at: int) (sizes: (int * int) list) =
            match sizes with
            | [] -> []
            | (i, size) :: rest -> (i, seq { at + 1 .. at + size } |> Seq.toList) :: (groups (at + size) rest)

        groups 0 (sizes |> Map.toList) |> Map.ofList

    let tiles = groups |> Map.toList |> List.map snd |> List.concat |> Set.ofList

    let cToV (c: char) =
        match c with
        | '.' -> _0
        | '#' -> tiles
        | '?' -> Set.union _0 tiles

    let strip = record |> List.map cToV
    let strip = [ [ _0 ]; strip; [ _0 ] ] |> List.concat

    let surrs = tiles |> Set.toList |> List.map (fun i -> Surr(i, groups))
    let lasts = groups |> Map.toList |> List.map snd |> List.map List.last |> Set.ofList

    let firsts =
        groups |> Map.toList |> List.map snd |> List.map List.head |> Set.ofList

    override this.ToString() =
        // $"Record({record} {sizes |> Map.toList} groups={groups |> Map.toList} tiles={tiles |> Set.toList}\n"
        let surrs = surrs |> List.map (fun s -> $"{s}") |> toSS

        let groups =
            groups
            |> Map.toList
            |> List.sort
            |> List.map snd
            |> List.map (List.map (fun i -> $"{i}"))
            |> List.map (String.concat "")
            |> String.concat " "

        let firsts = firsts |> toS
        let lasts = lasts |> toS
        $"Record {strip |> List.map toS |> toSS} groups={groups} surrs={surrs} f={firsts} l={lasts}"

    member this.GetSurrs() =
        surrs |> List.map (fun s -> (s.I), s) |> Map.ofList

    member this.Strip = strip
    member this.Check = check



let parse (line: string) =
    let [| chars; ints |] = line.Split(" ")
    let ints = ints.Split(",") |> Array.toList |> List.map int
    let chars = chars.ToCharArray() |> Array.toList
    Record(chars, ints)

let records = lines |> List.map parse

records.Head |> (printfn "%A")

type Resolution =
    | IMPOSSIBLE
    | SOLVED
    | UNSOLVED

type Strip(strip: Set<int> list, surrs: Map<int, Surr>, check: List<int>) =
    let allTokens = strip |> Set.unionMany

    member this.Strip = strip

    override this.ToString() =
        let surrs =
            surrs |> Map.toList |> List.map snd |> List.map (fun s -> $"{s}") |> toSS

        $"Strip {strip |> List.map toS |> toSS} surrs={surrs}"

    member this.ShortString() = $"Strip {strip |> toLS}"

    member this.ShrinkL() =
        let rec shrink (seen: Set<int>) (strip: Set<int> list) =
            // printfn $"shrink seen={seen |> toS} strip={strip |> toLS}"
            match strip with
            | pre :: e :: post :: rest ->
                let e =
                    e
                    |> Set.filter (fun i -> if i = 0 then true else pre.Contains(surrs[i].DirectBefore))

                let e =
                    e
                    |> Set.filter (fun i ->
                        if i = 0 then
                            true
                        else
                            (Set.difference (surrs[i].AllBefore) seen).IsEmpty)

                let seen = Set.union seen pre
                pre :: (shrink seen (e :: post :: rest))
            | _ -> strip

        let strip = shrink _0 strip
        Strip(strip, surrs, check)

    member this.ShrinkLMini() =
        let rec shrink (seen: Set<int>) (strip: Set<int> list) =
            // printfn $"shrink seen={seen |> toS} strip={strip |> toLS}"
            match strip with
            | pre :: e :: post :: rest ->
                let e =
                    e
                    |> Set.filter (fun i -> if i = 0 then true else pre.Contains(surrs[i].DirectBefore))
                // let e = e |> Set.filter (fun i -> if i = 0 then true else (Set.difference (surrs[i].AllBefore) seen).IsEmpty)
                let seen = Set.union seen pre
                pre :: (shrink seen (e :: post :: rest))
            | _ -> strip

        let strip = shrink _0 strip
        Strip(strip, surrs, check)


    member this.ShrinkR() =
        let strip = List.rev strip

        let rec shrink (seen: Set<int>) (strip: Set<int> list) =
            match strip with
            | pre :: e :: post :: rest ->
                let e =
                    e
                    |> Set.filter (fun i -> if i = 0 then true else pre.Contains(surrs[i].DirectAfter))

                let e =
                    e
                    |> Set.filter (fun i ->
                        if i = 0 then
                            true
                        else
                            (Set.difference (surrs[i].AllAfter) seen).IsEmpty)

                let seen = Set.union seen pre
                pre :: (shrink seen (e :: post :: rest))
            | _ -> strip

        let strip = shrink Set.empty strip |> List.rev
        Strip(strip, surrs, check)

    member this.ShrinkRMini() =
        let strip = List.rev strip

        let rec shrink (seen: Set<int>) (strip: Set<int> list) =
            match strip with
            | pre :: e :: post :: rest ->
                let e =
                    e
                    |> Set.filter (fun i -> if i = 0 then true else pre.Contains(surrs[i].DirectAfter))
                // let e = e |> Set.filter (fun i -> if i = 0 then true else (Set.difference (surrs[i].AllAfter) seen).IsEmpty)
                let seen = Set.union seen pre
                pre :: (shrink seen (e :: post :: rest))
            | _ -> strip

        let strip = shrink Set.empty strip |> List.rev
        Strip(strip, surrs, check)

    member this.checkOrderingL() =
        let rec taper (seen: int) (line: Set<int> list) =
            match line with
            | [] -> []
            | z :: rest when z = _0 -> _0 :: (taper seen rest)
            | a :: rest ->
                let a = a |> Set.filter (fun a -> a < (seen + 2))
                let seen = if a.IsEmpty then seen else max a.MaximumElement seen
                a :: (taper seen rest)

        let strip = taper 0 strip
        Strip(strip, surrs, check)

    member this.checkOrderingL2() =
        let rec check1 (tokens:int list) (strip:Set<int> list) =
            match strip with
            | []  when tokens.Length > 0 -> [Set.empty]
            | _ when tokens.IsEmpty -> strip 
            | t :: rest ->
                let t = t |> Set.filter (fun i -> i = 0 || i >= tokens.Head)
                let tokens = if t.Contains tokens.Head then tokens.Tail else tokens
                t :: check1 tokens rest
        let strip = check1 (allTokens |> Set.toList |> List.sort) strip 
        Strip (strip, surrs, check)
        
    member this.BasicValidOrdering() =
            let fxed = strip |> List.filter (fun s -> s.Count = 1)
                        |> List.filter (fun s -> s <> _0)
                        |> List.map (Set.minElement)
            let rec isSorted (l:int list) =
                match l with
                | a::b::_ when a >= b -> false
                | _::b::rest -> isSorted (b::rest)
                | _ -> true
            isSorted fxed 
            
    member this.RemoveSingle() =
        let single = strip |> List.filter (fun s -> s.Count = 1 && s <> _0) |> Set.unionMany
        // printfn $"single {single |> toS}"
        let clean (e: Set<int>) =
            if e.Count = 1 then e else Set.difference e single

        let strip = strip |> List.map clean
        Strip(strip, surrs, check)

    member this.DecideOnlyInstance() =
        let only1 =
            strip
            |> List.map Set.toList
            |> List.concat
            |> List.groupBy (fun i -> i)
            |> List.filter (fun (_, b) -> b.Length = 1)
            |> List.map fst
            |> Set.ofList
        // printfn $"Only 1: {only1 |> toS}"
        let clean (e: Set<int>) =
            let is = Set.intersect e only1
            if is.IsEmpty then e else is

        let strip = strip |> List.map clean
        Strip(strip, surrs, check)

    member this.IsSolved() =
        if strip |> List.exists (fun e -> e.Count <> 1) then
            false
        else
            let strip = strip |> List.map Set.minElement

            let unique =
                strip
                |> List.filter (fun i -> i > 0)
                |> (fun r -> (r |> List.distinct |> List.length) = r.Length)

            let rec groups (curr: int) (strip: int list) =
                match strip with
                | 0 :: rest -> curr :: (groups 0 rest)
                | _ :: rest -> groups (curr + 1) rest
                | [] -> [ curr ]

            let g = groups 0 strip |> List.filter (fun i -> i <> 0)

            if unique && (g <> check) then
                printfn $"WRONG: ${strip |> toL}"
                false
            else
                unique


    member this.Status() =
        if strip |> List.exists (_.IsEmpty) then
            IMPOSSIBLE
        elif strip |> List.exists (fun e -> e.Count <> 1) |> not then
            SOLVED
        else
            UNSOLVED

    member this.Expand() : Strip list =
        let i = strip |> List.findIndex (fun e -> e.Count > 1)
        let pre = strip |> List.take i
        let e = strip[i]
        let post = strip |> List.skip (i + 1)
        //        printfn $"{pre |> toLS} :: {e |> toS} :: {post |> toLS}"

        let e = e |> Set.toList

        let strips =
            e |> List.map (fun i -> [ pre; [ Set.singleton i ]; post ] |> List.concat)

        strips |> List.map (fun strip -> Strip(strip, surrs, check))

    member this.Check = check
    member this.Surrs = surrs


let strips =
    lines
    |> List.map parse
    |> List.map (fun r -> Strip(r.Strip, r.GetSurrs(), r.Check))

// printfn $"{lines.Head}"
let s = strips.Head

let solve (checkOrder: bool) (strip: Strip) =
    // printfn $"{strip}"
    let strip = strip.ShrinkL()
    // printfn $"{strip}"
    let strip = strip.ShrinkR()
    // printfn  $"{strip}"
    let strip = strip.RemoveSingle()
    // printfn $"{strip}"
    let strip = strip.DecideOnlyInstance()
    // printfn $"{strip.ShortString()} solved={strip.Status()}"
    let strip = if checkOrder then strip.checkOrderingL () else strip
    strip

// solve strips.Head
// strips |> List.map (printfn "%A")

let rec solveAll (strips: Strip list) : int =
    if strips.IsEmpty then
        0
    else
        let strips = List.map (solve true) strips
        let solved = strips |> List.filter _.IsSolved() |> List.length
        let unsolved = strips |> List.filter (fun s -> s.Status() = UNSOLVED)

        // printfn $"SOLVED: {solved}"

        // unsolved |> List.map (fun u -> printfn $"UNSOLVED: {u.ShortString()}")
        let expands = unsolved |> List.map (_.Expand()) |> List.concat
        // expands |> List.map (fun s -> printfn $"EXPANDED: {s.ShortString()}")
        // [solved;(solveAll expands )] |> List.concat
        solved + (solveAll expands)

// strips |> List.map solve

let stopWatch = System.Diagnostics.Stopwatch.StartNew()
let solutions = solveAll strips

stopWatch.Stop()
printfn $"TIME: {stopWatch.Elapsed.TotalMilliseconds}"

let checkSolution (strip: Strip) =
    let checks = strip.Check
    let strip = strip.Strip |> List.map (fun s -> s |> Set.minElement)

    let rec groups (curr: int) (strip: int list) =
        match strip with
        | 0 :: rest -> curr :: (groups 0 rest)
        | _ :: rest -> groups (curr + 1) rest
        | [] -> [ curr ]

    let g = groups 0 strip |> List.filter (fun i -> i <> 0)
    let valid = checks = g

    if valid then
        // printfn "VALID"
        ()
    else
        printfn $"checkSolution: strip = {strip |> toL} check={checks |> toL} groups={g |> toL} {valid}"

    valid

// let solutions2 = solutions |> List.filter checkSolution

printfn $"ANSWER: {solutions}"

// solutions |> List.map checkSolution

let parse2 (line: string) =
    let [| chars; ints |] = line.Split(" ")
    let chars = chars |> List.replicate 5 |> String.concat "?"
    let ints = ints.Split(",") |> Array.toList |> List.map int
    let ints = ints |> List.replicate 5 |> List.concat
    //    printfn $"parse2 {chars} {ints |> toL}"
    let chars = chars.ToCharArray() |> Array.toList
    Record(chars, ints)

let strips2 =
    lines
    |> List.map parse2
    |> List.map (fun r -> Strip(r.Strip, r.GetSurrs(), r.Check))

// strips2 |> List.map (printfn "%A")

// printfn $"{strips2.Head}"

// lines.Head |> parse2 // |> printfn "%A"

let s1 = strips2 |> List.last |> solve true

printfn $"s1 {s1.ShortString()}"

// let s2 = s1.checkOrderingL()

// printfn  $"s2 { s2.ShortString() }"

// let solutions2 = solveAll strips2

// printfn $"ANSWER 2: {solutions2}"

let splitByFixed (strip: Strip) =

    let rec split (strip: Set<int> list) =
        let con (e: Set<int>) = e.Count = 1 && e <> _0

        match strip with
        | z :: zz :: rest when z = _0 && zz = _0 -> split (zz :: rest)
        | z :: rest when z = _0 -> [ _0 ] :: split rest
        | e :: _ when con e ->
            let i = strip |> List.findIndex (fun i -> con i |> not)
            let pre = strip |> List.take (i)
            let rest = strip |> List.skip (i)
            pre :: (split rest)
        | _ :: _ ->
            let i = strip |> List.findIndex (fun i -> i.Count < 2)
            let pre = strip |> List.take (i)
            let rest = strip |> List.skip (i)
            pre :: (split rest)
        | [] -> []

    let sp = split strip.Strip
    let sp = sp |> List.filter (fun is -> is.Head.Count > 1)
    // printfn $"sp: {sp |> (fun l -> l |> List.map toLS ) |> toLP }"
    sp

// s1 |> splitByFixed

let rmc (strip: Strip) =
    let rec rmc (strip: Set<int> list) =
        match strip with
        | z :: zz :: rest when z = _0 && zz = _0 -> rmc (zz :: rest)
        | z :: a :: zz :: rest when z = _0 && zz = _0 && a.Count = 1 -> rmc (zz :: rest)
        | [] -> []
        | a :: rest -> a :: (rmc rest)

    Strip(rmc strip.Strip, strip.Surrs, strip.Check)

let splitz (strip: Strip) =
    let rec split (buff: Set<int> list) (multi: bool) (strip: Set<int> list) =
        match strip with
        | z :: rest when z = _0 ->
            let rest = split [] false rest
            if multi then (buff |> List.rev) :: rest else rest
        | [] -> if multi then [ (buff |> List.rev) ] else []
        | c :: rest when c.Count = 1 -> split (c :: buff) multi rest
        | v :: rest -> split (v :: buff) true rest

    split [] false strip.Strip
    |> List.filter (fun s -> s.Length > 0)
    |> List.map (fun s -> Strip(s, strip.Surrs, strip.Check))

let solve2 (strip: Strip) =
    let rec rsolve (strip: Strip) =
        let strip2 = strip |> solve true
        if strip2.Strip = strip.Strip then strip else rsolve strip2

    let strip = strip |> rsolve
    let segs = splitByFixed strip
    let segs = segs |> List.map (fun seg -> [ [ _0 ]; seg; [ _0 ] ] |> List.concat)
    let strips = segs |> List.map (fun seg -> Strip(seg, strip.Surrs, strip.Check))
    // printfn $"SOLVING(2) {strip}"
    strips |> List.map (fun sub -> printfn $" SUB> {sub.ShortString()}")

    if strips.IsEmpty then
        1
    else
        let strips = strips |> List.map (_.Expand())

        let rec subsolve (indent: int) (strip: Strip) =
            let status = strip.Status()

            if status = IMPOSSIBLE then
                0
            elif strip.BasicValidOrdering() |> not then
                printfn $"BAD ORDER: {strip.ShortString()}"
                0
            else
                let ind = "  " |> String.replicate indent
                let orig = strip
                // printfn $"{ind}SUBSolve {strip.ShortString()}"
                let strip = strip.DecideOnlyInstance()
                let strip = strip.RemoveSingle()
                // printfn $" -> {strip}"
                let strip = strip.ShrinkLMini()
                let strip = strip.ShrinkRMini()
                printfn $"{ind} => {strip.ShortString()}"
                // let strip = strip.checkOrderingL2()
                // printfn $"{ind} +> {strip.ShortString()}"

                if strip.Strip <> orig.Strip then
                    subsolve (indent + 1) strip
                else
                    // printfn $" =>> {strip}"
                    // printfn $" -> {strip.IsSolved()}"
                    // printfn $" -> {strip.Status()}"
                    match strip.Status() with
                    | IMPOSSIBLE ->
                        // printfn $"{ind}  IMPOSSIBLE -> 0"
                        0
                    | SOLVED ->
                        // printfn $"{ind}  SOLVED -> 1"
                        1
                    | UNSOLVED ->
                        let strips = strip.Expand()
                        let ress = strips |> List.map (subsolve (indent + 1))
                        // printfn $"ress {ress}"
                        ress |> List.sum

        let v = strips |> List.map (fun strips -> strips |> List.map (subsolve 0))
        let v = v |> List.map List.sum
        // printfn $"v = {v |> toL}"
        v |> List.reduce (*)

// strips |> List.map solve2 |> List.map (printfn "%A")

// let res = strips2 |> List.last |> solve2
// printfn $"res: {res}"


strips2 |> List.map solve2 |> List.map (int64) |> List.sum |> printfn "ANSWER 2: %A"
