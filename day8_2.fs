open System.IO 

exception Failure of string 

let fileName = "/Users/jan/Downloads/input8"

let lines = File.ReadAllLines fileName |> Seq.toList

lines |> List.map (printfn "%A")

type LLR (next: char list, orig: char list) =
    let next = if next = [] then orig else next
    let orig = orig
   
    member this.Direction = next.Head
    member this.Next() = LLR (next.Tail, orig)
    member this.LoopSize() = orig.Length |> int64
    
    override this.ToString() = $"LLR({next} {orig}...)"
    
let steps =
    let steps = lines.Head.ToCharArray() |> List.ofArray
    LLR (steps,steps)
    
printfn $"STEPS: dir = {steps.Direction} next = {steps.Next()}"

let parse (line: string) =
    let from = (line.Split("=")[0]).Trim()
    let line = line.Split("=")[1]
    let line = line.Replace("(","").Replace(")","").Split(",") |> Array.toList
    let line = line |> List.map (fun s -> s.Trim())
    (from, (line.Head, line.Tail.Head))
    
let desertMap = lines.Tail.Tail |> List.map parse |> Map.ofList

printfn $"desertMap = {desertMap}"

let nextLoc (curr: string) (steps: LLR) : string =
    let next = desertMap[curr]
    match steps.Direction with
    | 'L' -> fst next
    | 'R' -> snd next

let rec find (i:int64) (curr:string) (steps: LLR) =
    if curr = "ZZZ" then i
    else find (i+1L) (nextLoc curr steps) (steps.Next())

//let answer = find 0 "AAA" steps

//printfn $"ANSWER: {answer}"

let locations = desertMap.Keys |> Set.ofSeq 

let ends : Set<string> =
    let atEnd (s:string) = s.ToCharArray() |> Array.toList |> List.last 
    locations |> Set.filter (fun s -> atEnd s = 'Z') 
    
printfn $"ENDS: {ends}"
    
let start =
    let atEnd (s:string) = s.ToCharArray() |> Array.toList |> List.last 
    locations |> Set.filter (fun s -> atEnd s = 'A') 

printfn $"start = {start}"

let rec find2 (i:int64) (curr:string) (steps: LLR) =
    if ends.Contains curr then (i,curr,steps) 
    else find2 (i+1L) (nextLoc curr steps) (steps.Next())

let dists1 = start |> Set.toList |> List.map (fun s -> find2 0L s steps) 

printfn $"dists = {dists1}"

dists1 |> List.map (printfn "%A")

// let answer2 = dists |> Set.toList |> List.reduce (*)

// printfn $"ANSWER 2 = {answer2}"

let rec moveSteps (i:int64) (curr:string) (steps:LLR) =
    if i = 0 then curr
    else moveSteps (i-1L) (nextLoc curr steps) (steps.Next())
    
let dists = dists1 |> List.map (fun (a,_,_) -> a)

dists |> List.map (fun s -> (s, (s % steps.LoopSize()))) |> List.map (printfn "%A")

printfn $"LoopSize: : {steps.LoopSize()}"

let after (loc:string) =
    printfn "AFTER: "
    let next =
        match steps.Direction with
        | 'L' -> desertMap[loc] |> fst
        | 'R' -> desertMap[loc] |> snd 
    find2 1 next (steps.Next())
    
after "ZZZ" |> printfn "%A"

let dists2 = dists1 |> List.map (fun (dist,term,_) -> (dist,term,after term))

dists2 |> List.map (printfn "%A")

let dist = dists1 |> List.map (fun (a,_,_) -> a) |> List.map (fun i -> i / steps.LoopSize())

dist |> List.map (printfn "%A")
printfn $"DIST: {dist}"

let answer2 = (dist |> List.reduce (*)) * steps.LoopSize()

printfn $"answer2 = {answer2}"