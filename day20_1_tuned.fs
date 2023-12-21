open System.IO

exception Failure of string

let _0 = Set.singleton 0

let fileName = "/Users/jan/Downloads/input20.txt"

let lines = File.ReadAllLines fileName |> Seq.toList
// lines |> List.map (printfn "%s")

type Id = string

type Module =
    | Broadcaster of Id * Id list
    | FlipFlop of Id * bool * Id list
    | Conjunction of Id * Map<Id, bool> * Id list

let parse (line: string) =
    let line = line.Split("->")
    let mt = line[0].ToCharArray()[0]
    let name = line[0].Trim().Substring(1)
    let dests = line[1].Split(",") |> Array.toList
    let dests = dests |> List.map (_.Trim())

    match mt with
    | '%' -> FlipFlop(name, false, dests)
    | '&' -> Conjunction(name, Map.empty, dests)
    | 'b' -> Broadcaster(line[0].Trim(), dests)

let rec cables (modules: Module list) =
    match modules with
    | [] -> []
    | (FlipFlop(name, _, dests)) :: rest -> List.concat [ dests |> List.map (fun d -> (name, d)); cables rest ]
    | (Conjunction(name, _, dests)) :: rest -> List.concat [ dests |> List.map (fun d -> (name, d)); cables rest ]
    | (Broadcaster(name, dests)) :: rest -> List.concat [ dests |> List.map (fun d -> (name, d)); cables rest ]

let fillConj (modules: Module list) =
    let cables = cables modules
    // cables |> List.map (printfn "cable %A")

    let fill (m: Module) =
        match m with
        | Conjunction(name, _, dests) ->
            let inputs = cables |> List.filter (fun (_, d) -> d = name) |> List.map fst
            let inputs = inputs |> List.map (fun src -> src, false) |> Map.ofList
            (Conjunction(name, inputs, dests))
        | _ -> m

    modules |> List.map fill

let modules = lines |> List.map parse |> fillConj

// modules |> List.map (printfn "%A")

type Cables = (Id * Id * bool) list

let rec addMap (map: Map<Id, bool>) (values: (Id * bool) list) =
    match values with
    | [] -> map
    | nameval :: rest ->
        let map = map.Add nameval
        addMap map rest

let clockcycle (cables: Cables) (modules: Module list) =
    cables |> List.filter (fun (s,d,signal) -> d = "rx") |> List.map (fun (_,_,signal) -> $"RX: {signal}")
    let resolveM (m: Module) : Module * Cables =
        match m with
        | FlipFlop(name, value, dest) ->
            let flips =
                cables
                |> List.filter (fun (s, d, signal) -> d = name && (not signal))
                |> List.length

            if flips > 0 then
                FlipFlop(name, not value, dest), dest |> List.map (fun dest -> name, dest, not value)
            else
                m, []
        | Broadcaster(name, dests) ->
            let outputs =
                cables
                |> List.filter (fun (s, d, signal) -> d = name)
                |> List.map (fun (_, _, signal) -> dests |> List.map (fun dest -> (name, dest, signal)))
                |> List.concat

            m, outputs
        | Conjunction(name, memory, dests) ->
            let inputs = cables |> List.filter (fun (_, d, _) -> d = name)
            let updates = inputs |> List.map (fun (s, _, signal) -> (s, signal))

            let rec handleUpdates (memory: Map<Id,bool>) (updates: (Id*bool) list) : Map<Id,bool>*bool list=
                match updates with
                | [] -> memory,[]
                | update::updates ->
                    let memory = memory.Add update
                    let output = memory.Values |> Seq.contains false
                    let memory,outputs = handleUpdates memory updates
                    let outputs = output::outputs
                    memory,outputs
            let memory,outputs = handleUpdates memory updates
            let expand (output:bool) = dests |> List.map (fun dest -> (name,dest,output))
            let outputs = outputs |> List.map expand  |> List.concat
            Conjunction(name, memory, dests), outputs

    let modulesAndCables = modules |> List.map resolveM
    let modules = modulesAndCables |> List.map fst
    let cables = modulesAndCables |> List.map snd |> List.concat
    modules, cables

let first: Cables = [ ("button", "broadcaster", false) ]

let rec resolve (lows: int) (highs: int) (cables: Cables) (modules: Module list) =
     
    let lows = 
        lows + (cables |> List.filter (fun (_, _, signal) -> not signal) |> List.length)

    let highs =
        highs + (cables |> List.filter (fun (_, _, signal) -> signal) |> List.length)

    let modules, cables = clockcycle cables modules

    if cables.Length > 0 then
        resolve lows highs cables modules
    else
        lows, highs, modules



let rec solve (lows: int) (highs: int) (n: int) (modules: Module list) : bool * int * int =
    if n = 0 then
        false, lows, highs
    else
        let nlows, nhighs, modules = resolve 0 0 first modules

        let highs = highs + nhighs
        let lows = lows + nlows
        solve lows highs (n - 1) modules

let solve1000 =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let found, lows, highs = solve 0 0 1000 modules
    stopWatch.Stop()
    let t1 = stopWatch.Elapsed.TotalMilliseconds

    if found then
        printfn $"FOUND {1000 - lows}"
    else
        printfn $"SOLVE1000 {lows} {highs}"
        let lows = lows |> int64
        let highs = highs |> int64
        let res = lows * highs
        printfn $"ANSWER: {res}"
        printfn $"TIME: {t1}"

let rec findAllCables (modules:Module list) : (Id*Id) list =
    match modules with
    | [] -> []
    | FlipFlop(name,_,dests)::modules ->
        let these = dests |> List.map (fun dest -> (name,dest))
        let rest = findAllCables modules
        List.concat [these;rest]
    | Broadcaster(name,dests)::modules ->
        let these = dests |> List.map (fun dest -> (name,dest))
        let rest = findAllCables modules
        List.concat [these;rest]
    | Conjunction(name,_,dests)::modules ->
        let these = dests |> List.map (fun dest -> (name,dest))
        let rest = findAllCables modules
        List.concat [these;rest]
        
let allCables = findAllCables modules         

printfn "ALL CABLES"
allCables |> List.map (printfn "CABLE %A")
let solve2 =
    let values = Map.empty.Add ("broadcaster", 1L)
    let sources = allCables |> List.groupBy snd
                  |> List.map (fun (id, sources) -> (id, sources |> List.map fst))
                  |> Map.ofList
    sources |> Map.map (fun k v -> printfn $"{k} <- {v}")
    
     
    let rec resolve (values:Map<Id,int64>) (modules:Module list) : Map<Id,int64>*Module list =
        
    // values
    1
        
    
        