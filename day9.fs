open System.IO
open System.Numerics 

exception Failure of string 

let fileName = "/Users/jan/Downloads/input9"

let lines = File.ReadAllLines fileName |> Seq.toList |> List.map (fun s ->s.Split (" ") |> Array.toList |> List.map (int64))

lines |> List.map (printfn "%A")

let rec differences (readings: int64 list) =
    match readings with
    | a::b::rest -> (b-a) :: (differences (b::rest))
    | [a] -> []

lines |> List.map (fun l -> differences l) |> List.map (printfn "%A")
let rec values (it: int64 list list) =
    let zeroes it = it |> List.filter (fun s -> s <> 0L) |> List.isEmpty
    if zeroes it.Head then
        it
    else
        values ((differences it.Head) :: it) 
        
let pyramids = lines |> List.map (fun l -> values ([l]))
printfn "Pyramid 1:"

pyramids.Head |> List.map (printfn "%A")


let mirror (pyramid:int64 list list) = pyramid |> List.map List.rev 

let rec extrapolate (diff: int64) (rpyramid:int64 list list) =
    match rpyramid with
    | [] -> []
    | (a::row) :: rest ->
        let row = (a + diff) :: a :: row
        row :: (extrapolate row.Head rest) 

let rec extrapolateHead (diff: int64) (rpyramid:int64 list list) =
    match rpyramid with
    | [] -> []
    | (a::row) :: rest ->
        let row = (a-diff) :: a :: row
        row :: (extrapolateHead row.Head rest) 

let xp = pyramids |> List.map mirror |> List.map (extrapolate 0L) |> List.map mirror

xp |> List.map (fun p -> 
                printfn "Extrapolated: "
                p |> List.map (printfn "%A"))

let lasts = xp |> List.map List.last |> List.map List.last |> List.sum 

printfn $"{lasts}"

let xpr = pyramids |> List.map (extrapolateHead 0L) |> List.map mirror

let lastsr = xpr |> List.map List.last |> List.map List.last |> List.sum 

xpr |> List.map (fun p -> 
                printfn "ExtrapolatedHead: "
                p |> List.map (printfn "%A"))

printfn $"{lastsr}"

