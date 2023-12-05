open System.IO 

exception Failure of string 

let fileName = "/Users/jan/Downloads/input"

let lines = File.ReadAllLines fileName |> Seq.toList 

let seeds = lines.Head.Split(" ") |> Array.toList |> List.tail |> List.map int64

printfn $"SEEDS: {seeds}"
let lines1 = lines |> List.skip 2
printfn $"{lines1}"

type Range (source:int64, target:int64, length: int64) =
    let source = source
    let target = target
    let length = length
    member this.Contains (id:int64) =
        source <= id && (source + length - 1L) >= id
    member this.ValueOf (id:int64) =
        id - source + target
    override this.ToString() = $"Range({source},{target},{length})"

type Converter(sourceType:string, targetType: string, ranges: Range list) =
    let sourceType = sourceType
    let targetType = targetType
    let ranges = ranges
    member this.SourceType = sourceType
    member this.TargetType = targetType
    
    member this.TargetOf (sourceId:int64) =
        let ranges = ranges |> List.filter (fun r -> r.Contains sourceId)
        match ranges.Length with
        | 0 -> sourceId
        | 1 -> ranges.Head.ValueOf sourceId
        | _ -> raise (Failure $"Multiple ranges: {ranges}")
    
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

convs |> List.map (printfn "%A") 

let converterMap : Map<string, Converter list> =
    convs |> List.groupBy (fun c -> c.SourceType) |> Map.ofList
    
printfn $"{converterMap}"

let rec find (source:(string*int64)) : (string*int64) list =
    printfn $"find ({source})"
    let convs = converterMap[fst source] 
    let targets = convs |> List.map (fun c -> c.TargetType,c.TargetOf (snd source))
    // printfn $"FOUND: {targets}"
    let locations = targets |> List.filter (fun t -> fst t = "location") 
    // printfn $"locations: {locations}"
    let targets = targets |> List.filter (fun t -> fst t <> "location")
    let next = targets |> List.map find |> List.concat
    [ locations ; next ] |> List.concat  

let locations = seeds |> List.map (fun s -> find ("seed", s)) |> List.concat |> List.map snd 

printfn $"locatons: {locations}"

printfn $"Answer 1: {locations |> List.min}"