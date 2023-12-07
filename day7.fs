open System.IO 

exception Failure of string 

let fileName = "/Users/jan/Downloads/input7"

let lines = File.ReadAllLines fileName |> Seq.toList

lines |> List.map (printfn "%A")


let toCard (c:char) =
    match c with
    | 'A' -> 14L
    | 'K' -> 13L
    | 'Q' -> 12L
    | 'J' -> 11L
    | 'T' -> 10L
    | d -> d - '0' |> int64

let FIVE = 7L
let FOUR = 6L
let HOUSE = 5L
let THREE = 4L

let PAIRS = 3L
let PAIR = 2L
let TRASH = 1L

let toSortValue (t: int64, [a;b;c;d;e]: int64 list) =
    t * 100L * 100L * 100L * 100L * 100L +
    a * 100L * 100L * 100L * 100L +
    b * 100L * 100L * 100L +
    c * 100L * 100L +
    d * 100L +
    e

let toType (sortedCards: int64 list) =
    let sortedCards = sortedCards |> List.sort 
    match sortedCards with
    | [a;b;c;d;e] when a = e -> FIVE
    | [a;b;c;d;e] when a = d || b = e -> FOUR
    | [a;b;c;d;e] when a = c && d = e -> HOUSE
    | [a;b;c;d;e] when a = b && c = e -> HOUSE
    | [a;b;c;d;e] when a = c || b = d || e = c -> THREE 
    | [a;b;c;d;e] when a = b  && c = d -> PAIRS
    | [a;b;c;d;e] when a = b && d = e -> PAIRS
    | [a;b;c;d;e] when b = c && d = e -> PAIRS
    | [a;b;c;d;e] when a = b || b = c || c = d || d = e -> PAIR
    | a -> TRASH
    
let toJType (cards: int64 list) =
    let replaceJoker (cards: int64 list) (jcard:int64) =
        cards |> List.map (fun c -> if c = 1L then jcard else c)
    let r = seq { 2 .. 14 } |> Seq.toList
    let types = r |> List.map (fun jv -> replaceJoker cards (jv |> int64)) |> List.map toType
    types |> List.max

type Hand (cards: int64 list, bid:int64) =
    let cards = cards
    let bid = bid
    let typ = toType cards
    let jtype = toJType cards 
    let sortValue = toSortValue(typ, cards)
    
    let sortValueJ = toSortValue(jtype, cards)
    
    member this.SortValue = sortValue
    member this.SortValueJ = sortValueJ
    member this.Bid = bid
    
    member this.ToJokerHand() =
        Hand(cards |> List.map (fun c -> if c = 11 then 1L else c), bid)
    override this.ToString() = $"Hand({cards}[{typ}][{jtype}] {bid} [{sortValue},{sortValueJ}])"
    
let toHand (line:string) =
    let line = line.Split(" ")|> Array.toList
    let cards = line.Head.ToCharArray() |> Array.map toCard |> Array.toList
    let bid = line.Tail.Head |> int64
    Hand(cards,bid)
    
let hands = lines |> List.map toHand
hands |> List.map (printfn "%A")

let sortedHands = hands |> List.sortBy (fun c -> c.SortValue) 

let indexed = List.indexed sortedHands

indexed |> List.map (printfn "%A")
let scores = indexed |> List.map (fun (i,c) -> (i+1 |> int64) * c.Bid)

scores |> List.map (printfn "%A")

scores |> List.sum |> printfn "SUM: %A"

let jhands = hands |> List.map (fun h -> h.ToJokerHand())

let sortedHandsJ = jhands |> List.sortBy (fun c -> c.SortValueJ)

let indexedJ = List.indexed sortedHandsJ

let scoresJ = indexedJ |> List.map (fun (i,c) -> (i+1 |> int64) * c.Bid)

scoresJ |> List.map (printfn "%A")

scoresJ |> List.sum |> printfn "SUM J : %A"

// jhands |> List.map (printfn "%A")