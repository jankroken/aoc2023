open System.IO 

// this is obviously wrong (numbers infect eachother, but meh... it worked...)

let fileName = "/Users/jan/Downloads/input4"

let lines = File.ReadAllLines fileName |> Seq.toList 

printfn $"{lines}"

type Card(id:int, wins:int64 list, nums:int64 list) =
    let id = id
    let wins = wins
    let nums = nums
    override this.ToString() =
        $"Card({id}: {wins} | {nums} )"
    member this.Id = id
    member this.Score1 () =
        let rec ipow n i = if i = 0 then 1 else 2 * (ipow n (i-1))
        let wins = wins |> Set.ofList
        let nums = nums |> Set.ofList
        let nw = Set.intersect wins nums |> Set.count
        if nw = 0 then 0 else ipow 2 (nw - 1)
    member this.WinsCards() : Set<int> =
        let wins = wins |> Set.ofList
        let nums = nums |> Set.ofList
        let nw = Set.intersect wins nums |> Set.count
        let rec winsCards (nw: int) =
            if nw = 0 then []
            else (id+nw) :: (nw - 1 |> winsCards)
        let gets = winsCards nw  |> List.rev |> Set.ofList
//        printfn $"WinsCards: nw={nw} gets={gets}"
        gets
     member this.WinCount() : int =
        let wins = wins |> Set.ofList
        let nums = nums |> Set.ofList
        Set.intersect wins nums |> Set.count |> int
 
         

let parse (line:string) : Card =
    let line = line.Split(":")
    let id = line[0].Split(" ") |> Array.last |> int 
    let line = line[1].Split("|")
    let wins = line[0].Split(" ") |> Array.map (fun s -> s.Trim())
                                  |> Array.filter (fun s -> s <> "")
                                  |> Array.map int64 |> Array.toList
    let nums = line[1].Split(" ") |> Array.map (fun s -> s.Trim())
                                  |> Array.filter (fun s -> s <> "")
                                  |> Array.map int64 |> Array.toList
                              
    Card(id,wins,nums)
let cards = lines |> List.map parse

let score = cards |> List.map (fun c -> c.Score1()) |> List.sum 

cards |> List.map (fun c -> printfn $"{c} {c.Score1()}")

printfn $"Answer1 : {score}"

type CardWin (id:int, wins:Set<int>, cards: int list) =
    let id = id
    let wins = wins
    let cards = cards 
    member this.Id = id
    member this.Wins = wins
    member this.Cards = cards
    override this.ToString() =
        let wins = wins |> Set.toSeq |> Seq.map (fun i -> $"{i}") |> String.concat ""
        let cards = cards |> List.sort |> List.toSeq |> Seq.map (fun i -> $"{i}") |> String.concat "" 
        $"CardWin({id}: {wins}>{cards})"
    member this.Apply (dead:CardWin) =
        let cards = List.concat [cards;[dead.Id];dead.Cards]
        let wins = wins |> Set.remove dead.Id
        CardWin(id,wins,cards)
    member this.IsDeadEnd = wins.IsEmpty
    member this.IsAlive = wins.IsEmpty |> not
        

// let toCardWin (c:Card) : CardWin = CardWin(c.Id, c.WinsCards(),List.empty)

// let chain = cards |> List.map toCardWin
    
printfn "ATTEMPT 2:"

cards |> List.map (fun c -> printfn $"{c} -> {c.WinCount()}")

let counts = cards |> List.map (fun c -> 1L,c.WinCount())

let rec reduce (counts:(int64*int) list) =
    match counts with
    | [] -> 0L
    | (num,wins)::rest ->
        let incs = rest |> List.take wins
        let keep = rest |> List.skip wins
        let incs = incs |> List.map (fun (n,w) -> (n+num,w))
        incs |> List.map (printfn "INCS: %A")
        let rest = List.concat [incs;keep]
        num + (reduce rest)
        
printfn $"COUNTS: {counts}"

let a = reduce counts
printfn $"ANSWER2 = {a}"