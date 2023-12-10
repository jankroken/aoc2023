open System.IO
open System.Numerics 

exception Failure of string 

let fileName = "/Users/jan/Downloads/input10"

let lines = File.ReadAllLines fileName |> Seq.toList

type Pos = int64*int64

let parse (lines: string list) =
    let toPos((y,s):int*string):(Pos*char) list =
        let row = s.ToCharArray() |> Array.toList |> List.indexed
        row |> List.map (fun (x:int,c) -> (x |> int64,y |> int64),c)
    lines |> List.indexed |> List.map toPos 

let map = lines |> List.rev |> parse |> List.concat |> List.filter (fun (_,c) -> c <> '.') |> Map.ofList 

lines |> List.map (printfn "%A")

printfn $"{map}"

let start = map |> Map.filter (fun _ c -> c = 'S')  |> Map.toList |> List.head

printfn $"{start}"


let findNeighbours (map:Map<Pos,char>) (((x,y),c):Pos*char) =
    // printfn $"findNeighbours map={map} ({((x,y),c)})"
    let east = map.TryFind (x+1L,y)
    let west = map.TryFind (x-1L,y)
    let north = map.TryFind (x,y+1L)
    let south = map.TryFind (x,y-1L)
    // printfn $"findNeighbours E={east} W={west} N={north} S={south}"

    let east = if c = 'S' || c = '-' || c = 'L' || c = 'F' then Some((x+1L,y),east) else None
    let east =
        match east with
        | Some(p,Some('-')) -> Some(p,'-')
        | Some(p,Some('J')) -> Some(p,'J')
        | Some(p,Some('7')) -> Some(p,'7')
        | _ -> None

    let west = if c = 'S' || c = '-' || c = '7' || c = 'J' then Some((x-1L,y),west) else None
    let west =
        match west with
        | Some(p,Some('-')) -> Some(p,'-')
        | Some(p,Some('F')) -> Some(p,'F')
        | Some(p,Some('L')) -> Some(p,'L')
        | _ -> None

    let north = if c = 'S' || c = '|' || c = 'L' || c = 'J' then Some((x,y+1L),north) else None
    let north =
        match north with
        | Some(p,Some('F')) -> Some(p,'F')
        | Some(p,Some('7')) -> Some(p,'7')
        | Some(p,Some('|')) -> Some(p,'|')
        | _ -> None

    let south = if c = 'S' || c = '|' || c = 'F' || c = '7' then Some((x,y-1L),south) else None
    let south =
        match south with
        | Some(p,Some('L')) -> Some(p,'L')
        | Some(p,Some('J')) -> Some(p,'J')
        | Some(p,Some('|')) -> Some(p,'|')
        | _ -> None
       
    // printfn $"findNeighbours E={east} W={west} N={north} S={south}"
    
    let next = [north;south;east;west] |> List.filter Option.isSome |> List.map Option.get
    
    // printfn $"NEXT {next}"
    
    next 
    
let rec solve (i:int) (p:(Pos*char) list) (visited:Set<Pos>) =
    if i % 100 = 0 then printfn $"solve ({i})"
    let pos = p |> List.map fst |> Set.ofList
    let visited = Set.union visited pos 
    // printfn $"solve: p={p} rem={rem}"
    let next = p |> List.map (findNeighbours map) |> List.concat
               |> List.filter (fun (p,_) -> visited.Contains p |> not)
    // printfn $"next = {next}"
    if next = [] then
        printfn $"TERMINATED {i} VISITED:{visited}"
        visited
    else
        solve (i+1) next visited 
   
    
let visited = solve 0 [(fst start,'S')] Set.empty

let map2 = lines |> List.rev |> parse |> List.concat
           |> List.map (fun (p,c) -> if visited.Contains p then (p,c) else (p,'.'))
           |> Map.ofList
           
printfn $"MAP2 : {map2}"

let printMap (map:Map<Pos,char>) =
    let x2,y2 = map.Keys |> Seq.max
    let ys = seq {0L .. y2} |> Seq.toList
    let xs = seq {0L .. x2} |> Seq.toList
    let ch (x,y) =
        match map.TryFind (x,y) with
        | None -> ' '
        | Some(c) -> c
    let toString (cl: char list) =
        cl |> List.toArray |> System.String.Concat 
        
    ys |> List.map (fun y -> xs |> List.map (fun x -> ch(x,y)))
    |> List.map toString
    |> List.rev 
    |> List.map (printfn "%A")

printMap map2

let map3 = map2 |> Map.toList |> List.map (fun ((x,y),p) -> (x*2L,y*2L),p) |> Map.ofList

let fillMap (map:Map<Pos,char>) =
    let x2,y2 = map.Keys |> Seq.max
    let ys = seq {0L .. y2} |> Seq.toList
    let xs = seq {0L .. x2} |> Seq.toList
    let ch (x,y) =
        match map.TryFind (x,y) with
        | None -> '?'
        | Some(c) -> c
        
    ys |> List.map (fun y -> xs |> List.map (fun x -> (x,y),ch(x,y)))
    |> List.concat 
    |> Map.ofList

let map4 = fillMap map3

let connectMap (map:Map<Pos,char>) =
    let fix (((x,y),c):Pos*char) =
        if c <> '?' then (x,y),c
        else
            let l = map.TryFind (x-1L,y)
            let r = map.TryFind (x+1L,y)
            let u = map.TryFind (x,y+1L)
            let d = map.TryFind (x,y-1L)
            match (l,r,u,d) with
            | (Some('F'),_,_,_) -> (x,y),'#'
            | (Some('L'),_,_,_) -> (x,y),'#'
            | (Some('-'),_,_,_) -> (x,y),'#'
            | (_,Some('7'),_,_) -> (x,y),'#'
            | (_,Some('J'),_,_) -> (x,y),'#'
            | (_,Some('-'),_,_) -> (x,y),'#'
            | (_,_,Some('|'),_) -> (x,y),'#'
            | (_,_,Some('7'),_) -> (x,y),'#'
            | (_,_,Some('F'),_) -> (x,y),'#'
            | (_,_,_,Some('L')) -> (x,y),'#'
            | (_,_,_,Some('J')) -> (x,y),'#'
            | (_,_,_,Some('|')) -> (x,y),'#'
            | _ -> (x,y),c
    map |> Map.toList |> List.map fix |> Map.ofList 

let map5 = connectMap map4

let map6 = map5 |> Map.toList |> List.map (fun (p,c) -> if c = '?' then (p,'.') else (p,c)) |> Map.ofList

printfn ""
printfn "MAP 6"
printfn ""

printMap map6

let findBorderDots (map:Map<Pos,char>) =
    let x2,y2 = map.Keys |> Seq.max
    map |> Map.toList
    |> List.filter (fun ((x,y),c) -> (x = 0L || y = 0L || x = x2 || y = y2) && c = '.')
    |> List.map (fun (p,_) -> p) |> Set.ofList

printfn $"Borderdots: {findBorderDots map6}"


let waterFlow (map:Map<Pos,char>) =
    let water = findBorderDots map 
    let dots = map |> Map.toList |> List.filter (fun (p,c) -> c = '.')
               |> List.map fst |> List.filter (fun p -> water.Contains p |> not)
    let rec solve (dots:Pos list) (water:Set<Pos>) =
        printfn $"waterFlow.solve dots:{dots} water:{water}"
        let isWet ((x,y):Pos) =
            let ww = water.Contains(x-1L,y)
            let we = water.Contains(x+1L,y)
            let wn = water.Contains(x,y+1L)
            let ws = water.Contains(x,y-1L)
            ww || we || wn || ws
        let wet = dots |> List.filter isWet |> Set.ofList 
        let dry = dots |> List.filter (fun p -> isWet p |> not)
        let water = Set.union water wet
        printfn $"solve: wet = {wet} dry = {dry} water = {water}"
        if wet.IsEmpty then
            printfn $"Solved: dry={dry}"
            dry
        else
            solve dry water 
        
    let solved = solve dots water
    solved 
let dryTiles = waterFlow map6 |> List.filter (fun (x,y) -> (x % 2L, y % 2L) = (0L,0L)) |> List.map (fun (x,y) -> (x/2L,y/2L))

printfn $"dryTiles {dryTiles.Length} = {dryTiles}"