open System.IO 

// this is obviously wrong (numbers infect eachother, but meh... it worked...)

let fileName = "/Users/jan/Downloads/input3"

let lines = File.ReadAllLines fileName |> Seq.toList 

lines.Head |> printfn "FIRST LINE: %A"

let parse (lines:string list) =
    let toPairs (row:int,line:string) =
       let line = line.ToCharArray() |> Seq.indexed |> List.ofSeq
                  |> List.map (fun (col,char) -> (col,row),char)
       line
    let list = lines |> List.indexed |> List.map toPairs |> List.concat
               |> List.filter (fun (_,c) -> c <> '.')
    list |> Map.ofList
   
let cmap = lines |> parse

let digs = cmap |> Map.filter (fun _ c -> (c >= '0' && c <= '9')) 
let syms = cmap |> Map.filter (fun _ c -> (c <'0' || c > '9')) 

printfn $"CMAP> {cmap}"
printfn $"DIGS> {digs}"
printfn $"SUMS> {syms}"

let neighbours (x:int,y:int) =
    [(x-1,y-1);(x,y-1);(x+1,y-1);
     (x-1,y);          (x+1,y)
     (x-1,y+1);(x,y+1);(x+1,y+1)];

let rec findClean (clean:Set<int*int>) (taint:Set<int*int>) =
    let newtaint = taint |> Set.toList |> List.map neighbours |> List.concat
                   |> List.filter clean.Contains |> Set.ofList
    let clean = clean |> Set.filter (fun c -> newtaint.Contains c |> not)
    if newtaint.IsEmpty then clean
    else findClean clean newtaint

let clean = findClean (digs.Keys |> Set.ofSeq) (syms.Keys |> Set.ofSeq)

printfn "Clean: "
printfn $"{clean}"

let dirty = digs |> Map.filter (fun c _ -> clean.Contains c |> not) |> Map.keys 

let cleanc = digs |> Map.toList
             |> List.filter (fun (c,_) -> clean.Contains c)
             |> List.sortBy (fun ((x,y),_) -> (y,x))

let dirtyc = digs |> Map.toList
             |> List.filter (fun (c,_) -> dirty.Contains c)
             |> List.sortBy (fun ((x,y),_) -> (y,x))

printfn $"clean c: {cleanc}"
printfn $"dirty c: {dirtyc}"

let rec findNums (digs: ((int*int)*char) list) : char list list =
    match digs with
    | ((x1,y1),d1)::((x2,y2),_)::_ when y1 = y2 && x2 = x1 + 1 ->
        let l = (findNums digs.Tail)
        (d1::(l.Head))::l.Tail 
    | (_,d1)::rest -> [d1] :: (findNums rest)
    | [] -> []

let numc = findNums dirtyc

let toNum (l:char list) =
    l |> Array.ofList |> System.String.Concat  |> int64

let nums = numc |> List.map toNum

printfn $"numc: {nums}"

printfn $"answ: {nums |> List.sum}"

let gears = syms |> Map.filter (fun _ c -> c = '*')

printfn $"GEARS: {gears}"

let gclean = findClean (digs.Keys |> Set.ofSeq) (gears.Keys |> Set.ofSeq)
let gdirty = digs |> Map.filter (fun c _ -> gclean.Contains c |> not) |> Map.keys

let gdirtyc = digs |> Map.toList
             |> List.filter (fun (c,_) -> gdirty.Contains c)
             |> List.sortBy (fun ((x,y),_) -> (y,x))
             
printfn $"GDIRTYC: {gdirtyc}"

 
let rec groupNums (digs: ((int*int)*char) list) : ((int*int)*char) list list =
    match digs with
    | ((x1,y1),_)::((x2,y2),_)::_ when y1 = y2 && x2 = x1 + 1 ->
        let l = (groupNums digs.Tail)
        (digs.Head::(l.Head))::l.Tail 
    | x::rest -> [x] :: (groupNums rest)
    | [] -> []

let gdigs = groupNums gdirtyc

printfn $"GDIGS: {gdigs}"

let rec toGNums (gdigs:((int*int)*char) list) =
    let coords = gdigs |> List.map fst |> Set.ofList
    let value = gdigs |> List.map snd |> toNum
    (coords,value)

let gears2 = gears |> Map.keys |> Seq.toList


let gnums = gdigs |> List.map toGNums

printfn $"GEARS2: {gears2}"

printfn $"GNUMS: {gnums}"

let near (gear:(int*int)) ((dposs,_):Set<int*int>*int64) =
    let n = neighbours gear |> Set.ofList
    Set.intersect n dposs |> Set.isEmpty |> not

let gears3 = gears2 |> List.map (fun gear -> gnums |> List.filter (near gear) |> List.map snd)

printfn $"GEARS 3: {gears3}"

let gears4 = gears3 |> List.filter (fun l -> l.Length = 2)

printfn $"GEARS 4: {gears4}"

let ratios = gears4 |> List.map (fun [a;b] -> a*b)

printfn $"RATIOS: {ratios}"

printfn $"ANSWER 2 = {ratios |> List.sum}"


