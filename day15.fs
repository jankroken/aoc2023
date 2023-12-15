open System.IO

exception Failure of string

let _0 = Set.singleton 0

let fileName = "/Users/jan/Downloads/input15"

let line = File.ReadAllLines fileName |> Seq.head
let words = line.Split(',') |> Array.toList 
// words |> List.map (printfn "%A")

let hashC (curr:int) (c:char) =
    let ascii = c |> int
    let value = ((ascii + curr) * 17) % 256 
    // printfn $"{c} {ascii} {value}"
    value 

let hash (s:string) =
    let rec hash (curr:int) (cl:char list) =
        match cl with
        | [] -> curr
        | c::rest ->
            let curr = hashC curr c
            hash curr rest
    s.ToCharArray() |> Array.toList |> hash 0
    
let sum = words |> List.map hash |> List.sum

printfn $"ANSWER 1: {sum}"

type Op(label:string, op:char, param:int) =
    let box = hash label 
    member this.Label = label
    member this.Box = box 
    member this.Op = op
    member this.Param = param
    
    override this.ToString() =
        if op = '-' then $"Op({label}[{box}] -)"
        else $"Op({label}[{box}] ={param})"
    
let parse (s:string) : Op =
    if s.Contains "-" then Op(s.Split("-")[0],'-',0)
    
    else
        let s = s.Split('=')
        Op(s[0],'=',s[1] |> int)
    
let ops = words |> List.map parse

type Lens = string*int
    
let rawBoxes : Map<int,Lens list> = seq { 0 .. 255 } |> Seq.toList |> Seq.map (fun n -> n,[]) |> Map.ofSeq

type Boxes (boxes:Map<int,Lens list>) =
    member this.Boxes = boxes
    override this.ToString() =
        let boxes = boxes |> Map.toList |> List.filter (fun (l,v) -> v.Length > 0)
        $"Boxes { boxes }"
        
    member this.Perform (op: Op) : Boxes =
        let v = boxes[op.Box]
        let v =
            if op.Op = '-' then
                v |> List.filter (fun (label,_) -> label <> op.Label) 
            else
                let i = v |> List.tryFindIndex (fun (label,_) -> label = op.Label)
                match i with
                | None -> [v;[(op.Label,op.Param)]] |> List.concat
                | Some(i) ->
                    let pre = v |> List.take i
                    let post = v |> List.skip (i+1)
                    [pre;[(op.Label,op.Param)];post] |> List.concat 
        let boxes = boxes.Add (op.Box, v) 
        Boxes(boxes)
        
let boxes = Boxes(rawBoxes)

let b1 = boxes.Perform (ops.Head)

printfn $"Boxes: {b1}"

let rec performAll (boxes:Boxes) (ops:Op list) =
    match ops with
    | [] -> boxes
    | op::ops ->
        let boxes = boxes.Perform op
        printfn $"performAll: op: {op}"
        printfn $"performAll: boxes = {boxes}"
        performAll boxes ops 

let getLenses (boxes:Boxes) =
    printfn "getLenses: "
    let boxes = boxes.Boxes |> Map.toList
    let boxes = boxes |> List.map (fun (i,l) -> (i+1,l |> List.indexed |> List.map (fun (i,l) -> (i+1,l))))
    let boxes = boxes |> List.map (fun (i,l) -> l |> List.map (fun l -> (i,l))) |> List.concat 
    let boxes = boxes |> List.map (fun (b,(s,(_,l))) -> (b |>int64,s|>int64,l|>int64))
    let boxes = boxes |> List.map (fun (b,s,l) -> (b*s*l))
    printfn $"{boxes}"
    boxes |> List.sum 
    
let lenses = performAll boxes ops |> getLenses

printfn $"ANSWER 2 : {lenses}"