open System.IO

exception Failure of string

let _0 = Set.singleton 0

let fileName = "/Users/jan/Downloads/input"

let lines = File.ReadAllLines fileName |> Seq.toList
// lines |> List.map (printfn "%s")

type Dir = L | R | U | D
type Color = string
type Pos = int*int 

type Instruction(dir:Dir, steps:int, color: Color) =
    member this.Dir = dir
    member this.Steps = steps
    member this.Color = color 
    override this.ToString() = $"({dir} {steps} {color}"
    
let parse (line:string) =
    let line = line.Split(" ")
    let dir =
        match line[0][0] with
        | 'L' -> L
        | 'R' -> R
        | 'U' -> U
        | 'D' -> D
    let steps = line[1] |> int
    let color = line[2]
    Instruction(dir,steps,color)

let instructions = lines |> List.map parse

instructions |> List.map (printfn "%A")


type Holes = Map<Pos,Color>

let rec dig (holes:Holes) (pos:Pos) (instructions:Instruction list) =
    match instructions with
    | [] -> holes
    | inst :: instructions ->
        let holes = holes.Add (pos,inst.Color)
        if inst.Steps > 0 then
            let pos =
                match pos,inst.Dir with
                | ((x,y),R) -> (x+1,y)
                | ((x,y),L) -> (x-1,y)
                | ((x,y),U) -> (x,y+1)
                | ((x,y),D) -> (x,y-1)
            let inst = Instruction(inst.Dir, inst.Steps - 1,inst.Color)
            dig holes pos (inst::instructions)
        else
            dig holes pos instructions

let ground = dig Map.empty (0,0) instructions
let untouched =
    let minX = ground.Keys |> Seq.map fst |> Seq.min
    let maxX = ground.Keys |> Seq.map fst |> Seq.max
    
    let minY = ground.Keys |> Seq.map snd |> Seq.min
    let maxY = ground.Keys |> Seq.map snd |> Seq.max
    
    printfn $"min,max = {(minX,minY)} {(maxX,maxY)}"
    
    seq { minX - 1 .. maxX + 1 }
    |> Seq.map (fun x -> seq { minY - 1 .. maxY + 1 } |> Seq.map (fun y -> x,y))
    |> Seq.concat
    |> Seq.filter (fun p -> ground.ContainsKey p |> not)
    |> Set.ofSeq 

let rec waterflow (poss:Set<Pos>) (wet:Pos list) =
    let wet = wet |> List.filter poss.Contains
    if wet = [] then poss
    else
        let poss = Set.difference poss (wet |> Set.ofList)
        let wet =
            wet |> List.map (fun (x,y) -> [(x-1,y);(x+1,y);(x,y-1);(x,y+1)])
            |> List.concat |> List.distinct
        printfn $"wet = {wet}"
        waterflow poss wet  

// bordered |> Map.toList |> List.map (printfn "%A")

let internals = waterflow untouched [untouched.MinimumElement]

internals.Count + ground.Keys.Count |> printfn "ANSWER 1: %A"
            
// untouched |> Set.toList |> List.map (printfn "%A")