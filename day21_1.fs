open System.IO

exception Failure of string

let _0 = Set.singleton 0

let fileName = "/Users/jan/Downloads/input"

let lines = File.ReadAllLines fileName |> Seq.toList
lines |> List.map (printfn "%s")

type Pos = int64*int64

let parse (lines: string list) =
    let lines = lines |> List.rev |> List.indexed
    let parse ((y,line): int*string) =
        let line = line.ToCharArray() |> Array.toList |> List.indexed 
        line |> List.map (fun (x,c) -> ((x |> int64,y |> int64),c))
    lines |> List.map parse |> List.concat |> Map.ofList
    
let farm = lines |> parse

let guardenSpots =
    farm |> Map.toList |> List.filter (fun (_,c) -> c = '.')
    |> List.map (fun (pos,_) -> pos)
    |> Set.ofList

let startSpot : Pos =
    farm |> Map.toList |> List.filter (fun (_,c) -> c = 'S') |> List.head |> fst 
    
farm |> printfn "%A"

let rec addAllToMap (map:Map<Pos,int64>) (n:int64) (poss: Pos list) =
    match poss with
    | [] -> map
    | pos::poss ->
        let map = map.Add(pos,n)
        addAllToMap map n poss 

let rec waterflow (n:int64) (wet:Set<Pos>) (reachable:Set<Pos>) (reachableSteps:Map<Pos,int64>) (guardenSpots:Set<Pos>) =
    if wet.IsEmpty then
        reachable,reachableSteps 
    else
        let guardenSpots = Set.difference guardenSpots wet
        let reachable = Set.union reachable wet
        let reachableSteps = addAllToMap reachableSteps n (wet |> Set.toList )
        let neighbours =
            wet |> Set.toList
            |> List.map (fun ((x,y):Pos) -> [(x+1L,y);(x-1L,y);(x,y+1L);(x,y-1L)])
            |> List.concat |> Set.ofList
        let wet = Set.intersect neighbours guardenSpots
        waterflow (n+1L) wet reachable reachableSteps guardenSpots

let reachableSpots,reachableSteps = waterflow 0 (Set.singleton startSpot) Set.empty Map.empty guardenSpots

reachableSteps |> (printfn "%A")

let findInSteps (n:int64) (reachableSteps:Map<Pos,int64>) =
    reachableSteps |> Map.toList
    |> List.map snd
    |> List.filter (fun steps -> steps <= n)
    |> List.filter (fun steps -> steps % 2L = n % 2L)
    |> List.length
 
let answerFor6 = findInSteps 6L reachableSteps

printfn $"answer for 6 = {answerFor6}"

let answerFor64 = findInSteps 64L reachableSteps

printfn $"answer for 64 = {answerFor64}"