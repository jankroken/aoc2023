open System.IO

exception Failure of string

let _0 = Set.singleton 0

let fileName = "/Users/jan/Downloads/input19"

let lines = File.ReadAllLines fileName |> Seq.toList
lines |> List.map (printfn "%s")

type Rule =
    | Send of string
    | GT of char*int64*string
    | LT of char*int64*string 

let parseRule(rule:string) =
    let rule = rule.Split(":")
    if  rule.Length = 1 then
        Send rule[0]
    else
        let dest = rule[1]
        let rule = rule[0].Split("<")
        if rule.Length = 2 then
            LT (rule[0].ToCharArray()[0], rule[1] |> int64, dest)
        else
            let rule = rule[0].Split(">")
            GT (rule[0].ToCharArray()[0], rule[1] |> int64, dest)
let parsePart(part:string) : Map<char,int64> =
    let part = part.Split("{")[1]
    let part = part.Split("}")[0]
    let part = part.Split(",") |> Array.toList
    let parseComp (comp:string) : char*int64 =
        let comp = comp.Split("=")
        (comp[0].ToCharArray()[0]),(comp[1] |> int64)
    let part = part |> List.map parseComp
    part |> Map.ofList
    // printfn $"PART {part}"

let parse (lines: string list) : Map<string,Rule list>*Map<char,int64> list =
    let workflows = lines |> List.takeWhile (fun s -> s <> "")
    let parts = lines |> List.skipWhile (fun s -> s <> "") |> List.tail
    
    let parseWorkflow (line : string) : string*Rule list =
        let line = line.Split("{")
        let name = line[0]
        let rules = line[1].Split("}")[0]
        let rules = rules.Split(",") |> Array.toList
        let rules = rules |> List.map parseRule 
        printfn $"RULE ({name} > {rules}"
        name,rules 
    
    let workflows = workflows |> List.map parseWorkflow
    let workflows = workflows |> Map.ofList 
    let parts = parts |> List.map parsePart 
    workflows,parts 
    
    
let workflows,parts = lines |> parse

workflows |> printfn "%A"
parts |> printfn "%A"

let testRule (rule:Rule, part:Map<char,int64>) : Option<string> =
    match rule with
    | Send dest -> Some(dest)
    | GT (c,i,d) -> if part[c] > i then Some(d) else None
    | LT (c,i,d) -> if part[c] < i then Some(d) else None

let rec testRules ((rule::rules):Rule list) (part:Map<char,int64>) : string =
    match testRule (rule,part) with
    | Some(d) -> d
    | None -> testRules rules part 
    
let rec testPart (rule:Rule list) (part:Map<char,int64>) : bool =
    match testRules rule part with
    | "A" -> true
    | "R" -> false
    | dest ->
        let rule = workflows[dest]
        testPart rule part

let accepted = parts |> List.filter (fun p -> testPart (workflows["in"]) p)

accepted |> List.map (printfn "ACCEPTED %A")

let partScore (part:Map<char,int64>) = part.Values |> Seq.toList |> List.sum

let answer = accepted |> List.map partScore |> List.sum

printfn $"ANSWER: {answer}"

type Range = Map<char,int64*int64>

let allValues : Range = ['x',(1L,4000L);'m',(1L,4000L);'a',(1L,4000L);'s',(1L,4000L)] |> Map.ofList

let ruleSplit (rule:Rule, range:Range) : (Range*(string option)) list = 
    match rule with
    | Send dest -> [range,Some dest] 
    | GT (c,i,d) ->
        if (fst range[c] > i) then [range,Some d]
        elif (snd range[c] < i+1L) then [range,None]
        else 
            let rejected = (fst range[c]),i
            let passed = i+1L,(snd range[c])
            let rejected = range.Add (c,rejected)
            let passed = range.Add (c,passed)
            [rejected,None; passed,Some d] 
    | LT (c,i,d) ->
        if (snd range[c] < i) then
            printfn $"MATCH ALL {i} {range[c]}"
            [range,Some d]
        elif (fst range[c] >= i) then
            printfn $"MATCH NONE {i} {range[c]}"
            [range,None]
        else 
            let rejected = (i, snd range[c])
            let passed = (fst range[c]),(i-1L)
            let rejected = range.Add (c,rejected)
            let passed = range.Add (c,passed)
            [rejected,None; passed,Some d]
            
let rec rulesSplit ((rule::rules):Rule list) (range:Range) : (Range*string) list =
    let ranges = ruleSplit (rule,range)
    printfn $"Ranges {ranges}"
    ranges |> List.map (fun r -> (fst r),"banana")
    let determined = ranges |> List.filter (fun (r,od) -> od.IsSome) |> List.map (fun (r,od) -> r,od.Value)
    let others = ranges |> List.filter (fun (r,od) -> od.IsNone) |> List.map fst
    let determined2 = others |> List.map (fun range -> rulesSplit rules range) |> List.concat
    let determined = [determined;determined2] |> List.concat
    determined 

let scoreRange (range:Range) =
    let range = range |> Map.toList |> List.map snd
    let range = range |> List.map (fun (a,b) -> b-a+1L)
    range |> List.reduce (*)

let rec narrow (workflow:string) (values: Range) : int64 =
    let rule = workflows[workflow]
    let split = rulesSplit rule values 
    printfn $"NARROW: {rule}"
    let rejected = split |> List.filter (fun (_,dest) -> dest = "R") |> List.map fst
    let accepted = split |> List.filter (fun (_,dest) -> dest = "A") |> List.map fst 
    let toSend = split |> List.filter (fun (_,dest) -> dest <> "R" && dest <> "A")
    rejected |> List.map (printfn "REJECTED %A")
    accepted |> List.map (printfn "ACCEPTED %A")
    toSend |> List.map (printfn "TOSEND %A")
    let accepted = accepted |> List.map scoreRange |> List.sum 
    let accepted2 = toSend |> List.map (fun (range,dest) -> narrow dest range) |> List.sum 
    accepted+accepted2
    
    
let accepted2 = narrow "in" allValues

accepted2 |> (printfn "A2: %A")
printfn "AC: 167409079868000"
    

    