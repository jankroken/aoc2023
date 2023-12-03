open System.IO 

let fileName = "/Users/jan/Downloads/input2.txt"

let lines = File.ReadAllLines fileName |> Seq.toList 

printfn $"{1}"

type Draw (color: string, num: int64) =
     let color = color
     let num = num
     
     override this.ToString() = $"({color}:{num})"
     member this.Color = color
     member this.Num = num

type Draws(num:int64, draws: Draw list list) =
     let num = num
     let draws = draws
     
     override this.ToString() = $"{num}: {draws}"
     
     member this.Merge() =
          let sum (draws: Draw list) =
               draws |> List.map (fun (d:Draw) -> d.Num) |> List.sum 
          let draws = draws |> List.concat
          let draws = draws |> List.groupBy (fun (draw:Draw) -> draw.Color)
          let draws = draws
                      |> List.map (fun (n, draws) -> (n, draws |> sum)) |> Map.ofList
          printfn($"Merge: {draws}")
          (num,draws)

     member this.Max() =
          let max (draws: Draw list) =
               draws |> List.map (fun (d:Draw) -> d.Num) |> List.max
          let draws = draws |> List.concat
          let draws = draws |> List.groupBy (fun (draw:Draw) -> draw.Color)
          let draws = draws
                      |> List.map (fun (n, draws) -> (n, draws |> max)) |> Map.ofList
          printfn($"Max: {draws}")
          (num,draws)
let x = Draw("red",4)

let parse (line:string) =
     let toDraw (str:string) =
          let str = str.Split(" ") |> Array.toList
          // $"{str}" 
          Draw(str[2],str[1] |> int64)
     let line = line.Split(":")
     let lineNum = line[0].Split(" ")[1] |> int64
     let draws = line[1].Split(";") |> Array.toList
     let draws = draws |> List.map (fun hand -> hand.Split(",") |> Array.toList |> List.map toDraw)
     Draws(lineNum,draws)
     // printfn $"DRAW({lineNum} = {draws}"


let draws = lines |> List.map parse

printfn $"{draws}"
printfn("Max:")
let max = draws |> List.map (fun draws -> draws.Max())
printfn("Merge:")
let sums = draws |> List.map (fun draws -> draws.Merge())

let valid1((num:int64,max:Map<string,int64>)) =
     let red = max["red"]
     let blue = max["blue"]
     let green = max["green"]
     red <= 12 && green <= 13 && blue <= 14
     
let answer1 = max |> List.filter valid1 |> List.map fst |> List.sum

let power (draws:Draws) =
     draws.Max() |> snd |> Map.values |> Seq.toList |> List.reduce (*)
     //  printfn ($"power: {max}")
     // max
     

let powers = draws |> List.map power |> List.sum 

printfn($"Valids: {answer1}")

printfn($"powers: {powers}")