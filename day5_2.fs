open System.IO 

exception Failure of string 

let fileName = "/Users/jan/Downloads/input5"

let lines = File.ReadAllLines fileName |> Seq.toList 

let seedsn = lines.Head.Split(" ") |> Array.toList |> List.tail |> List.map int64
let rec toSeeds (seedsn:int64 list) =
    match seedsn with
    | f :: t :: rest -> (f,f+t-1L) :: toSeeds rest
    | [] -> []

let seeds = toSeeds seedsn

printfn $"SEEDS: {seeds}"
let lines1 = lines |> List.skip 2
// printfn $"{lines1}"

type Range (source:int64, target:int64, length: int64) =
    let source = source
    let sourceLast = source + length - 1L
    let target = target
    let length = length
    member this.ValueOf ((first,last):(int64*int64)) : ((int64*int64) list)*((int64*int64) list) =
        
        let contains = first <= (source+length-1L) && last >= source
        printfn $"ValueOf ({first},{last}) contains={contains}"
        if contains then
            let before = if first < source then [first,source-1L] else []
            let after = if last > sourceLast then [sourceLast+1L,last] else []
            let nomatch = [before;after] |> List.concat
            let targetFirst = (max first source) + target - source
            let targetLast = (min last sourceLast) + target - source 
            [targetFirst,targetLast],nomatch 
        else 
            [],[first,last]
        
    override this.ToString() = $"Range({source},{target},{length})"

type Converter(sourceType:string, targetType: string, ranges: Range list) =
    let sourceType = sourceType
    let targetType = targetType
    let ranges = ranges
    member this.SourceType = sourceType
    member this.TargetType = targetType
    
    member this.TargetsOf (ids:(int64*int64) list) : (int64*int64) list =
        printfn $"TargetsOf: {ids} ranges:{ranges}"
        let rec find (ranges:Range list) (ids:(int64*int64) list) : (int64*int64) list =
            match ranges with
            | [] -> ids
            | r :: ranges ->
                let res = ids |> List.map r.ValueOf
                let matches = res |> List.map fst |> List.concat 
                let nones = res |> List.map snd |> List.concat 
                printfn $"res: matches: {matches} nones={nones}"
                if nones.IsEmpty then matches
                else [matches ; (find ranges nones)] |> List.concat
        find ranges ids 
    
    override this.ToString() = $"Converter ({sourceType}->{targetType} {ranges})"

let rec parse (source:string) (target:string) (ranges:Range list) (lines: string list) : Converter list =
    match lines with
    | [] -> [Converter (source, target, ranges)] 
    | "" :: rest ->
        Converter (source, target, ranges) :: parse "?" "?" [] rest
    | s :: rest when s.Contains("-to-") ->
        let s = s.Split(" ") |> Array.head
        let s = s.Split("-")
        parse (s[0]) (s[2]) [] rest
    | tsl :: rest ->
        let tsl = tsl.Split(" ") |> Array.map int64
        let ranges = Range(tsl[1],tsl[0],tsl[2]) :: ranges 
        parse source target ranges rest
 
let convs = parse "?" "?" [] lines1

// convs |> List.map (printfn "%A") 

let converterMap : Map<string, Converter list> =
    convs |> List.groupBy (fun c -> c.SourceType) |> Map.ofList
    
// printfn $"{converterMap}"

let rec find (source:(string*((int64*int64) list))) : (int64*int64) list =
    printfn $"find ({source})"
    let convs = converterMap[fst source] 
    let targets = convs |> List.map (fun c -> c.TargetType,c.TargetsOf (snd source))
    printfn $"FOUND: {targets}"
    let locations = targets |> List.filter (fun t -> fst t = "location") 
    printfn $"locations: {locations}"
    let targets = targets |> List.filter (fun t -> fst t <> "location")
    printfn $"targets: {targets}"
    let deeper = targets |> List.map find |> List.concat 
    printfn $"deeper: {deeper}"
    let locations = locations |> List.map snd |> List.concat
    [locations ; deeper] |> List.concat 

let locs = find ("seed",seeds) |> List.map fst |> List.sort |> List.head

printfn $"LOCS: {locs}"

// let locations = seeds |> List.map (fun s -> find ("seed", s)) |> List.concat |> List.map snd 

// printfn $"locatons: {locations}"

// printfn $"Answer 1: {locations |> List.min}"