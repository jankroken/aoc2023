open System.IO 

exception Failure of string 

let fileName = "/Users/jan/Downloads/input6"

let lines = File.ReadAllLines fileName |> Seq.toList

// lines |> List.map (printfn "%A")

let times = lines.Head.Split(" ") |> Array.toList |> List.tail |> List.filter (fun s -> s <> "") |> List.map int64
let dist  = lines.Tail.Head.Split(" ") |> Array.toList |> List.tail |> List.filter (fun s -> s <> "") |> List.map int64

printfn $"TIMES: {times}"
printfn $"DIST: {dist}"

// TASK 1

let races = List.zip times dist

printfn $"RACES: {races}"

let rec timeRange (depth:int) ((t,d):int64*int64) ((mi,ma):int64*int64) =
    // printfn $"RACE: time:{t} dist:{d} curr:[{mi},{ma}]"
    if mi > ma then
        0L
    else 
    // let floor = d / t + 1L
    // let roof = t - floor 
    // printfn $"RF: {floor}->{roof}"
        let distmin = (t-mi) * mi
        let distmax = (t-ma) * ma
    
        let shortmin = d - distmin
        let shortmax = d - distmax 
   
        let minstep = if shortmin > 0 then shortmin / (t-mi) + 1L else 0L
    // let maxstep = if shortmax < 0 then shortmax / (t-ma) else 0
        let maxstep = if shortmax > 0 then shortmax / ma + 1L else 0L
   
        // printfn $"RES: {distmin}({shortmin}){distmax}({shortmax}) ADJ: {minstep} {maxstep}"
        if minstep > 0 || maxstep > 0 then
            timeRange (depth+1) (t,d) (mi+minstep,ma-maxstep)
        else
            let answer = ma - mi + 1L
            printfn $"ANSWER: {answer} depth:{depth}"
            answer
        
     
    
timeRange 1 (races.Head) (0L,fst races.Head)

printfn "RUN"

let ways = races |> List.map (fun r -> timeRange 1 (fst r,(snd r) + 1L) (0L,fst r))

let answer = ways |> List.reduce (*)

printfn $"WAYS {ways} > {answer}"

let time2 = lines.Head.Split(":") |> Array.tail |> Array.head |> (fun s -> s.Replace(" ","")) |> int64
let dist2 = lines.Tail.Head.Split(":") |> Array.tail |> Array.head |> (fun s -> s.Replace(" ","")) |> int64

printfn $"RACE 2 : {time2} {dist2}"

let stopWatch = System.Diagnostics.Stopwatch.StartNew()
let res2 = timeRange 1 (time2,dist2+1L) (0L,time2)

stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

printfn $"RES2 {res2}"


