open System.IO 

exception Failure of string 

let fileName = "/Users/jan/Downloads/input"

let lines = File.ReadAllLines fileName |> Seq.toList

lines |> List.map (printfn "%A")

type LLR (next: char list, orig: char list) =
    let next = if next = [] then orig else next
    let orig = orig
   
    member this.Direction = next.Head
    member this.Next() = LLR (next.Tail, orig)
    
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
    printfn $"nextLoc {curr} {desertMap}"
    let next = desertMap[curr]
    match steps.Direction with
    | 'L' -> fst next
    | 'R' -> snd next

let rec find (i:int64) (curr:string) (steps: LLR) =
    if curr = "ZZZ" then i
    else find (i+1L) (nextLoc curr steps) (steps.Next())

let answer = find 0 "AAA" steps

printfn $"ANSWER: {answer}"