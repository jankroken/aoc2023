open System.IO
open System.Numerics 

exception Failure of string 

let fileName = "/Users/jan/Downloads/input11"

let lines = File.ReadAllLines fileName |> Seq.toList |> List.map (fun s -> s.ToCharArray() |> Array.toList)

lines |> List.map (printfn "%A")

let expand (lines: char list list) =
    let expand (line:char list) : char list list =
        if line |> (List.contains '#') then
            [line]
        else 
            [line;line]
    let rec expandV (lines: (char list*char list) list) =
         if lines |> List.head |> snd = [] then lines |> List.map fst |> List.map List.rev 
         else 
         let expandColumn = lines |> List.map snd |> List.map List.head |> List.contains '#' |> not 
         let keep (lines: (char list)*(char list)) = (fst lines) |> (List.append [(snd lines |> List.head)]),(snd lines |> List.tail)
         let ex (lines: (char list)*(char list)) = (fst lines) |> (List.append [(snd lines |> List.head);(snd lines |> List.head)]),(snd lines |> List.tail)
         let ff = if expandColumn then ex else keep 
         lines |> List.map ff |> expandV   
         
    let lines = lines |> List.map expand |> List.concat |> List.map (fun l -> [],l) |> expandV
    
    lines 
    
let lines2 = expand lines

lines2 |> List.map (printfn "%A")


let toMap (lines:char list list) : ((int*int)*char) list list =
    let lines = lines |> List.map List.indexed |> List.indexed
    lines |> List.map (fun (y,xlines) -> xlines |> List.map (fun (x,c) -> ((x,y),c)))

let galaxies = lines2 |> List.rev |> toMap |> List.rev |> List.concat
               |> List.filter (fun (_,c) -> c = '#')
               |> List.map fst

printfn $"{galaxies}"

let rec allPairs (l: _ list) =
    match l with
    | [] -> []
    | h::t -> (t |> List.map (fun v -> (h,v))) :: (allPairs t)

let pairs = allPairs galaxies |> List.concat 
 
printfn $"allPairs: {pairs.Length} {pairs}"

let dist (((x1,y1),(x2,y2)): ((int*int)*(int*int))) =
    let xd = x1 - x2 |> abs
    let yd = y1 - y2 |> abs
    xd + yd 

let dist2 (((x1,y1),(x2,y2)): ((int64*int64)*(int64*int64))) =
    let xd = x1 - x2 |> abs
    let yd = y1 - y2 |> abs
    xd + yd 

List.map dist pairs |>  List.sum |> printfn "ANSWER: %A"


let expand2 (lines: char list list) =
    let XSIZE = 1000000L
    let rec xs (i:int64) (lines: char list list) =
        match lines with
        | [] -> [] 
        | (a::_) when a |> List.contains '#' -> (i) :: (xs (i+1L) lines.Tail)
        | _ -> (i) :: (xs (i+XSIZE) lines.Tail) 
    let rec ys (i:int64) (lines: char list list) =
        if lines.Head = [] then []
        else 
            let heads = lines |> List.map List.head
            printfn $"HEADS: {heads}"
            if heads |> List.contains '#' then
                printfn $"- {i} {i+1L}"
                (i) :: (lines |> List.map List.tail |> (ys (i+1L)))
            else
                printfn $"* {i} {i+XSIZE}"
                (i) :: (lines |> List.map List.tail |> (ys (i+XSIZE)))
    let y = xs 0L lines
    let x = ys 0L lines 
   
    x |> List.map (printfn "X: %A")
    y |> List.map (printfn "Y: %A")
    
    let rec mapC (cs: int64 list) (galaxies: _ list) =
        if cs = [] then []
        else (cs.Head,galaxies.Head) :: (mapC cs.Tail galaxies.Tail)
    
    let lines = lines |> List.map (mapC x) |> mapC y
    
    let injectY ((y,row):int64*(int64*char) list) =
        row |> List.map (fun (x,c) -> ((x,y),c))
    let lines = lines |> List.map injectY 
     
    lines |> List.map (printfn "%A")
    lines 
            
let galaxies2 = lines |> List.rev |> expand2 |> List.rev |> List.concat
                |> List.filter (fun (p,c) -> c = '#')
                |> List.map fst 

galaxies |> List.map (printfn "%A")
galaxies2 |> List.map (printfn "%A")

let pairs2 = galaxies2 |> allPairs |> List.concat  |> List.map dist2

pairs2 |> List.sum |> printfn "ANSWER2: %A"

let maxy = galaxies2 |> List.map snd |> List.max
let maxx = galaxies2 |> List.map fst |> List.max

let ys = seq { 0L .. maxy } |> Seq.toList
let xs = seq { 0L .. maxx } |> Seq.toList

let g = galaxies2 |> Set.ofList

// ys |> List.map (fun y -> xs |> List.map (fun x -> if g.Contains (x,y) then printf "#" else printf "."); printfn "")