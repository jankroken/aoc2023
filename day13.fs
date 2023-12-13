open System.IO

let lines = File.ReadAllLines "/Users/jan/Downloads/input13" |> Array.toList

lines |> List.map (printfn "%A")

let rec split (lines:string list) =
    match lines |> List.tryFindIndex (fun s -> s = "") with
    | Some(n) ->
        let block = lines |> List.take n
        let rest = lines |> List.skip (n+1)
        block :: (split rest)
    | None -> [lines]

let blocks1 = split lines

blocks1 |> List.map (printfn "%A")


let parseBlock (lines:string list) =
    let lines = lines |> List.indexed
    let row ((y,line):int*string) =
        let line = line.ToCharArray() |> Array.toList |> List.indexed
        line |> List.map (fun (x,c) -> (x,y),c)
    let lines = lines |> List.map row
    lines |> List.concat |> Map.ofList 
        
let blocks = blocks1 |> List.map parseBlock

printfn $"{blocks}"

let findHMirror (block:Map<int*int,char>) =
    let rocks = block |> Map.toList
                |> List.filter (fun (_,c) -> c = '#')
                |> List.map fst |> Set.ofList 
    let (mx,my) = block.Keys |> Seq.max
    let cand = seq {1 .. mx} |> Seq.toList
    let ys = seq {0 .. my} |> List.ofSeq 
    printfn $"cand = {cand}"
    let testCand (x:int) =
        let width = min x (mx-x+1)
        let dxs = seq { 1 .. width } |> Seq.toList
        printfn $"testCand {x} w:{width} {dxs}"
        let isRefl (dx,y) = block[x-dx,y] = block[x+dx-1,y]
        let isDiff p = isRefl p |> not 
        let rec testAll (ys:int list) (dxs:int list) =
            let ys = ys |> Seq.ofList
            let cs = ys |> Seq.map (fun y -> dxs |> Seq.ofList |> Seq.map (fun x -> x,y))
                     |> Seq.concat 
            let diff = cs |> Seq.tryFind isDiff
            diff = None 
        testAll ys dxs
    cand |> List.filter testCand 

let findVMirror (block:Map<int*int,char>) =
    let block =
        block |> Map.toList
        |> List.map (fun ((x,y),c) -> (y,x),c)
        |> Map.ofList
    findHMirror block 

printfn $"Blocks: {blocks.Length}"
       
findHMirror blocks.Head  |> printfn "Winner H: %A"

findVMirror blocks.Tail.Head  |> printfn "Winner V: %A"

let sumBlock (block:Map<int*int,char>) =
    let horz = findHMirror block
    let verz = findVMirror block
    let sumh = horz |> List.sum
    let sumv = verz |> List.map (fun i -> i * 100) |> List.sum
    let sum = sumh + sumv
    printfn $"sumBlock {horz} {verz} -> {sumv} + {sumh} = {sum}"
    sum
    
blocks |> List.map sumBlock |> List.sum |> printfn "ANSWER: %A"

let findHSmudge (block:Map<int*int,char>) =
    let rocks = block |> Map.toList
                |> List.filter (fun (_,c) -> c = '#')
                |> List.map fst |> Set.ofList 
    let (mx,my) = block.Keys |> Seq.max
    let cand = seq {1 .. mx} |> Seq.toList
    let ys = seq {0 .. my} |> List.ofSeq 
//    printfn $"cand = {cand}"
    let testCand (x:int) =
        let width = min x (mx-x+1)
        let dxs = seq { 1 .. width } |> Seq.toList
//        printfn $"testCand {x} w:{width} {dxs}"
        let isRefl (dx,y) = block[x-dx,y] = block[x+dx-1,y]
        let isDiff p = isRefl p |> not 
        let rec testAll (ys:int list) (dxs:int list) =
            let ys = ys |> Seq.ofList
            let cs = ys |> Seq.map (fun y -> dxs |> Seq.ofList |> Seq.map (fun x -> x,y))
                     |> Seq.concat 
            let cs = cs |> Seq.filter isDiff |> Seq.truncate 2
            let cs = cs |> Seq.toList
            if cs.Length = 1 then
                let (dx,y) = cs.Head
                printfn $"ANS: x={x} (dx,y)={(dx,y)}"
                // Some((x-dx,y),x)
                Some(x)
            else None 
        testAll ys dxs
    cand |> List.map testCand |> List.filter Option.isSome |> List.map Option.get

let findVSmudge (block:Map<int*int,char>) =
    let block =
        block |> Map.toList
        |> List.map (fun ((x,y),c) -> (y,x),c)
        |> Map.ofList
    findHSmudge block // |> List.map (fun (y,x) -> (x,y))
    

let hsmudge = blocks.Tail.Head |> findHSmudge
let vsmudge = blocks.Tail.Head |> findVSmudge

hsmudge |> printfn "H %A"
vsmudge |> printfn "V %A"

let fixSmudge (block:Map<(int*int),char>) =
    let smudges1 = findHSmudge block
    let smudges2 = findVSmudge block
    let smudge = [smudges1;smudges2] |> List.concat
    if smudge.Length <> 1 then
        printfn $"ERROR: found {smudge.Length} smudges"
        block 
    else
        let smudge = smudge.Head
        // let c = block[smudge]
        // let c = if c = '.' then '#' else '.'
        // block.Add (smudge,c)
        block 

let sumSmudge (block:Map<(int*int),char>) =
    let h = findHSmudge block
    let v = findVSmudge block
    let hs = h |> List.sum
    let vs = v |> List.map (fun i -> i * 100) |> List.sum 
    printfn $"hv = {hs} {vs}"
    hs + vs 

// let newBlocks = blocks |> List.map fixSmudge

// sumBlock newBlocks.Tail.Head |> printfn "%A"

blocks |> List.map sumSmudge |> List.sum |> printfn "%A"