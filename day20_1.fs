open System.IO

exception Failure of string

let _0 = Set.singleton 0

let fileName = "/Users/jan/Downloads/input20"

let lines = File.ReadAllLines fileName |> Seq.toList
lines |> List.map (printfn "%s")

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
    printfn $"parse {mt} {name} -> {dests}"

    match mt with
    | '%' -> FlipFlop(name, false, dests)
    | '&' -> Conjunction(name, Map.empty, dests)
    | 'b' -> Broadcaster(line[0].Trim(), dests)

let rec cables (modules: Module list) =
    match modules with
    | [] -> []
    | (FlipFlop(name, _, dests)) :: rest ->
            List.concat [ dests |> List.map (fun d -> (name, d)); cables rest ]
    | (Conjunction(name, _, dests)) :: rest -> 
            List.concat [ dests |> List.map (fun d -> (name, d)); cables rest ]
    | (Broadcaster(name, dests)) :: rest ->
        List.concat [ dests |> List.map (fun d -> (name, d)); cables rest ]

let fillConj (modules: Module list) =
    let cables = cables modules
    cables |> List.map (printfn "cable %A")

    let fill (m: Module) =
        match m with
        | Conjunction(name, _, dests) ->
            let inputs = cables |> List.filter (fun (_, d) -> d = name) |> List.map fst
            let inputs = inputs |> List.map (fun src -> src, false) |> Map.ofList
            (Conjunction(name, inputs, dests))
        | _ -> m

    modules |> List.map fill

let modules = lines |> List.map parse |> fillConj

modules |> List.map (printfn "%A")

type Cables = (Id * Id * bool) list

let rec addMap (map: Map<Id, bool>) (values: (Id * bool) list) =
    match values with
    | [] -> map
    | nameval :: rest ->
        let map = map.Add nameval
        addMap map rest

let clockcycle (cables: Cables) (modules: Module list) =
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
            if updates.IsEmpty then
                m, []
            else
                let memory = addMap memory updates
                let output = memory.Values |> Seq.contains false
                // printfn $"updates: {updates}"
                let outputs = dests |> List.map (fun dest -> name, dest, output )
                let outputs = outputs |> List.replicate updates.Length |> List.concat 
                Conjunction(name, memory, dests), outputs

    let modulesAndCables = modules |> List.map resolveM
    // modulesAndCables |> List.map (printfn "resolved: %A")
    let modules = modulesAndCables |> List.map fst
    let cables = modulesAndCables |> List.map snd |> List.concat
    modules, cables

let first: Cables = [ ("button", "broadcaster", false) ]

let rec resolve (lows:int) (highs:int) (cables:Cables) (modules:Module list) =
    let lows = lows + ( cables |> List.filter (fun (_,_,signal) -> not signal) |> List.length)
    let highs = highs + ( cables |> List.filter (fun (_,_,signal) -> signal) |> List.length)
    let modules, cables = clockcycle cables modules
    // modules |> List.map (printfn "RESOLVE: MODULE: %A")
    // cables |> List.map (printfn "RESOLVE: CABLE: %A")
    if cables.Length > 0 then
        resolve lows highs cables modules
    else
        lows,highs,cables,modules



let rec solve (lows:int) (highs:int) (n:int) (modules:Module list) : int*int =
    if n = 0 then
        lows,highs
    else 
        let nlows,nhighs,cables,modules = resolve 0 0 first modules
        let highs = highs + nhighs
        let lows = lows + nlows
        // printfn $"NLOWS = {nlows} NHIGHS = {nhighs} LOWS:{lows} HIGHS:{highs}"
        solve lows highs (n-1) modules
    
let solve1000 =
    let lows,highs = solve 0 0 1000 modules
    printfn $"SOLVE1000 {lows} {highs}"
    let lows = lows |> int64
    let highs = highs |> int64
    let res = lows * highs
    printfn $"ANSWER: {res}"