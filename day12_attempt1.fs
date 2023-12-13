open System.IO
open System.Numerics 

exception Failure of string 

let _0 = Set.singleton 0

let fileName = "/Users/jan/Downloads/input"

let lines = File.ReadAllLines fileName |> Seq.toList 

let toS (s:Set<int>) =
    s |> Set.toList |> List.map (fun f -> $"{f}") |> String.concat "" 
let toSS (s:string list) = s |> String.concat " "

let toLS (ls:Set<int> list) =
    ls |> List.map toS |> String.concat " "
    

lines |> List.map (printfn "%A")

type Surr(i:int, map: Map<int,int list>) =
    let i = i
    let g = map |> Map.toList |> List.filter (fun (_,b) -> b |> List.contains i) |> List.head |> fst
    let line = map[g]
    let index = line |> List.findIndex (fun e -> e = i)
    let directBefore =
        if index = 0 then 0 
        else line[index-1]
    
    let directAfter =
        if index = line.Length - 1 then 0
        else line[index + 1]
        
    let allBefore =
        let ing = line |> List.takeWhile (fun c -> c <> i) |> Set.ofList
        let gs = map |> Map.toList |> List.filter (fun (gn,_) -> gn < g)
        let gs = gs |> List.map snd |> List.concat |> Set.ofList
        Set.union ing gs
    let allAfter =
        let ing = line |> List.skip (index+1) |> Set.ofList
        let gs = map |> Map.toList |> List.filter (fun (gn,_) -> gn > g)
        let gs = gs |> List.map snd |> List.concat |> Set.ofList
        Set.union ing gs
        
    member this.I = i
    member this.DirectBefore = directBefore
    member this.DirectAfter = directAfter
    member this.AllBefore = allBefore
    member this.AllAfter = allAfter
    
    override this.ToString() =
        let allBefore = allBefore |> toS
        let allAfter = allAfter |> toS 
        
        $"({allBefore}<{directBefore}[{i}]{directAfter}>{allAfter})" 

type Record(record:char list,check:int list) =
    let sizes = check |> List.indexed |> Map.ofList
    let keys = sizes.Keys |> Set.ofSeq
    let record = record

    let groups =
        let rec groups (at:int) (sizes: (int*int) list) =
            match sizes with
            | [] -> []
            | (i,size)::rest -> (i,seq {at+1 .. at+size} |> Seq.toList) :: (groups (at+size) rest)
        groups 0 (sizes |> Map.toList) |> Map.ofList 
        
    let tiles = groups |> Map.toList |> List.map snd |> List.concat |> Set.ofList
    
    let cToV (c:char) =
        match c with
        | '.' -> _0 
        | '#' -> tiles
        | '?' -> Set.union _0 tiles 
        
    let strip = record |> List.map cToV
    let strip = [[_0];strip;[_0]] |> List.concat
    
    let surrs = tiles |> Set.toList |> List.map (fun i -> Surr(i, groups))
    let lasts = groups |> Map.toList |> List.map snd |> List.map List.last |> Set.ofList
    let firsts = groups |> Map.toList |> List.map snd |> List.map List.head |> Set.ofList 
    
    override this.ToString() =
        // $"Record({record} {sizes |> Map.toList} groups={groups |> Map.toList} tiles={tiles |> Set.toList}\n"
        let surrs = surrs |> List.map (fun s -> $"{s}") |> toSS
        let groups = groups |> Map.toList  |> List.sort |> List.map snd |> List.map (List.map (fun i -> $"{i}")) |> List.map (String.concat "") |> String.concat " "
        let firsts = firsts |> toS
        let lasts = lasts |> toS 
        $"Record {strip |> List.map toS |> toSS} groups={groups} surrs={surrs} f={firsts} l={lasts}"
        
    member this.GetSurrs() = surrs |> List.map (fun s -> (s.I),s) |> Map.ofList
    member this.Strip = strip 
        
        
           
let parse (line: string) =
    let [|chars;ints|] = line.Split(" ")
    let ints = ints.Split(",") |> Array.toList |> List.map int
    let chars = chars.ToCharArray() |> Array.toList
    Record(chars,ints)
    
let records = lines |> List.map parse

records.Head |> (printfn "%A")


type Strip(strip:Set<int> list, surrs:Map<int,Surr>) =
    
    member this.Strip = strip 
    member this.CleanSingleFromLeft () =
        let rec clean (seen:Set<int>) (prev:Set<int>) (strip:Set<int> list) =
//            printfn $"clean seen:{seen |> toS} prev:{prev |> toS} {strip |> toLS}"
            match strip with
            | [] -> []
//            | _0::rest when prev = _0 -> clean seen _0 rest
            | curr::rest ->
                // printfn $"clean:curr = {curr}"
                let curr = curr |> Set.filter (fun c -> if c = 0 then true else prev |> Set.contains surrs[c].DirectBefore)
                let seen = Set.union seen curr
                curr :: (clean seen curr rest) 
        let strip = clean _0 _0 strip
        Strip(strip, surrs)

    member this.CleanSingleFromRight () =
        let rec clean (seen:Set<int>) (prev:Set<int>) (strip:Set<int> list) =
            // printfn $"clean seen:{seen} prev:{prev} {strip}"
            match strip with
            | [] -> []
//          | _0::rest when prev = _0 -> clean seen _0 rest
            | curr::rest ->
                // printfn $"clean:curr = {curr}"
                let curr = curr |> Set.filter (fun c -> if c = 0 then true else prev |> Set.contains surrs[c].DirectAfter)
                let seen = Set.union seen curr
                curr :: (clean seen curr rest) 
        let strip = strip |> List.rev
        let strip = clean _0 _0 strip
        let strip = strip |> List.rev 
        Strip(strip, surrs)
    
    member this.CleanSeenFromLeft () =
        let seenAllBefore (i:int) (seen:Set<int>) =
            if i = 0 then true
            else
                let before = surrs[i].AllBefore
                let seen = Set.intersect before seen
                // printfn $"seenAllBefore: {i} {seen |> toS} {before |> toS}"
                seen.Count = before.Count 
            
        let rec clean (seen:Set<int>) (strip:Set<int> list) =
            match strip with
            | [] -> []
            | curr::rest ->
                // printfn $"CURR::REST = {curr} {rest}"
                let curr = curr |> Set.filter (fun i -> if i = 0 then true else seenAllBefore i seen)
                let seen = Set.union seen curr
                curr :: (clean seen rest)
        let strip = clean _0 strip
        Strip(strip, surrs)

    member this.CleanSeenFromRight () =
        let seenAllAfter (i:int) (seen:Set<int>) =
            if i = 0 then true
            else
                let before = surrs[i].AllAfter
                let seen = Set.intersect before seen
                // printfn $"seenAllAfter: {i} {seen |> toS} {before |> toS}"
                seen.Count = before.Count 
            
        let rec clean (seen:Set<int>) (strip:Set<int> list) =
            match strip with
            | [] -> []
            | curr::rest ->
                // printfn $"CURR::REST = {curr |> toS} {rest |> List.map toS |> toSS}"
                let curr = curr |> Set.filter (fun i -> if i = 0 then true else seenAllAfter i seen)
                let seen = Set.union seen curr
                curr :: (clean seen rest)
        let strip = strip |> List.rev
        let strip = clean _0 strip
        let strip = strip |> List.rev 
        Strip(strip, surrs)
        
    member this.ShrinkSingles() =
        let singles =
            strip |> List.map Set.toList |> List.concat
            |> List.groupBy (fun f -> f)
            |> List.filter (fun p -> snd p |> fun l -> l.Length = 1)
            |> List.map fst
            |> Set.ofList 
        let shrinkC (c:Set<int>) =
            let inter = Set.intersect singles c
            if inter.Count = 1 then inter else c
        let strip = strip |> List.map shrinkC
        Strip(strip, surrs)
    
    member this.RemoveIdentified() =
        let ids =
            strip |> List.filter (fun s -> s.Count = 1) |> List.filter (fun s -> s <> _0)
            |> Set.unionMany
        let rmIds (s:Set<int>) =
            if s.Count = 1 then s
            else s |> Set.filter (fun i -> ids.Contains i |> not)
        let strip = strip |> List.map rmIds
        Strip(strip,surrs)

    member this.IsSolved() = strip |> List.exists (fun s -> s.Count <> 1) |> not
    member this.IsInvalid() = strip |> List.exists (_.IsEmpty)
    
    member this.IsMany() =
        let hasAll = strip |> List.exists (fun s -> s.IsEmpty) |> not  
        let hasMany = strip |> List.exists (fun s -> s.Count > 1)
        hasAll && hasMany 
    
    member this.Expand() =
        let i = strip |> List.findIndex (fun s -> s.Count > 1)
        // printfn $"Expand:1 {strip |> toLS}"
        let pre = strip |> List.take i
        let post = strip |> List.skip (i+1)
        let at = strip[i]
        // printfn $"Expand:2 {pre |> toLS} : {at |> toS} : {post |> toLS}"
        let lists = at |> Set.toList |> List.map (fun e -> [pre;[Set.singleton e];post] |> List.concat)
        // lists |> List.map (fun l -> printfn $"Expand:3 {l |> toLS}")
        lists |> List.map (fun strip -> Strip(strip,surrs))
            
    override this.ToString() =
        let surrs = surrs |> Map.toList |> List.map snd |> List.map (fun s -> $"{s}") |> toSS
        $"Strip {strip |> List.map toS |> toSS} surrs={surrs}"
        
    member this.ShortString() =
        $"Strip {strip |> List.map toS |> toSS}"
        
let strips = records |> List.map (fun r -> Strip(r.Strip, r.GetSurrs()))

// strips |> List.map (printfn "%A")

let solve (strip:Strip) =
    printfn $"strip: {strip.ShortString()}"
    let strip = strip.CleanSingleFromLeft()
    printfn $"clean:L1: {strip.ShortString()}"
    let strip = strip.CleanSingleFromRight()
    printfn $"clean:R1: {strip.ShortString()}"
    let strip = strip.CleanSeenFromLeft()
    // printfn $"clean LS: {strip}"
    let strip = strip.CleanSeenFromRight()
    // printfn $"clean RS: {strip}"
    let strip = strip.ShrinkSingles()
    printfn $"clean SS: {strip}"
    let strip = strip.RemoveIdentified()
    printfn $"clean ID: {strip}"
    strip 
    
printfn ""
printfn ""

printfn $"{lines.Tail.Tail.Head}"
printfn $"{records.Tail.Tail.Head}"
solve strips.Tail.Tail.Head

printfn "*************************"
printfn "ALL"
printfn "*************************"
 
let solved = strips |> List.map solve

solved |> List.map (printfn "%A")

printfn $"{solved.Tail.Head}" 
let s = solved.Tail.Head.ShrinkSingles()
printfn $"{s}"

printfn "****  ****"
solved |> List.map (fun s -> printfn $"{s.ShortString()}"; printfn $"{s.RemoveIdentified().ShortString()} solved:{s.IsSolved()} invalid:{s.IsInvalid()}")



printfn "##### LAST #####"

let lastLine = lines |> List.last
let lastRecord = records |> List.last
let lastStrip = strips |> List.last

// printfn $"{lastLine}"
// printfn $"{lastRecord}"
// printfn $"{lastStrip}"

// solve lastStrip

let rec solveR (strip:Strip) =
    let solved = solve strip
    if solved.Strip = strip.Strip then strip
    else solveR solved 

let rec recSolve (strips:Strip list) : int =
    if strips.Length = 0 then 0
    else 
        let strips = strips |> List.map solveR
        let solvedCount = strips |> List.filter (fun s -> s.IsSolved()) |> List.length
        let invalid = strips |> List.filter (fun s -> s.IsInvalid())
        printfn $"recSolve: solved:{solvedCount} invalid:{invalid.Length}"
        let strips = strips |> List.filter (fun s -> s.IsMany())
        let strips = strips |> List.map (fun l -> l.Expand()) |> List.concat 
        strips |> List.map (fun s -> printfn $"recSolve: new: {s.ShortString()})")
        solvedCount + (recSolve strips) 
    
let c = recSolve solved

// let c = recSolve [strips.Head]

printfn $"ANSWER: {c}"

// strips.Head |> printfn "%A"

// solved.Head.Expand()