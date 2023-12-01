open System.IO 

let fileName = "/Users/jan/Downloads/input2.txt"

let lines = File.ReadAllLines fileName |> Seq.toList 

let rec firstDigit (line:char list) =
    match line |> Seq.toList with
    | (a::rest) when a >= '0' && a <= '9' -> a 
    | 'o'::'n'::'e'::rest -> '1' 
    | 't'::'w'::'o'::rest -> '2' 
    | 't'::'h'::'r'::'e'::'e'::rest -> '3' 
    | 'f'::'o'::'u'::'r'::rest -> '4' 
    | 'f'::'i'::'v'::'e'::rest -> '5'
    | 's'::'i'::'x'::rest -> '6'
    | 's'::'e'::'v'::'e'::'n'::rest -> '7' 
    | 'e'::'i'::'g'::'h'::'t'::rest -> '8'
    | 'n'::'i'::'n'::'e'::rest -> '9'
    | _::rest -> firstDigit rest

let rec lastDigitR (line:char list) =
    match line |> Seq.toList with
    | (a::rest) when a >= '0' && a <= '9' -> a 
    | 'e'::'n'::'o'::rest -> '1' 
    | 'o'::'w'::'t'::rest -> '2' 
    | 'e'::'e'::'r'::'h'::'t'::rest -> '3' 
    | 'r'::'u'::'o'::'f'::rest -> '4' 
    | 'e'::'v'::'i'::'f'::rest -> '5'
    | 'x'::'i'::'s'::rest -> '6'
    | 'n'::'e'::'v'::'e'::'s'::rest -> '7' 
    | 't'::'h'::'g'::'i'::'e'::rest -> '8'
    | 'e'::'n'::'i'::'n'::rest -> '9'
    | _::rest -> lastDigitR rest

let toNum1 line =
    let line = line |> Seq.toList |> List.filter (fun a -> a >= '0' && a <= '9')
    let h = line |> List.head
    let t = line |> List.last
    $"{h}{t}" |> int64
    
    
let toNum line =
    let first = line |> Seq.toList |> firstDigit
    let last = line |> Seq.toList |> List.rev |> lastDigitR
    $"{first}{last}" |> int64
// let digs = lines |> List.map Seq.toList |> List.map (fun l -> $"{firstDigit l }")

let digs1 = lines |> List.map toNum1 |> List.sum 
let digs = lines |> List.map toNum |> List.sum 

// let digs = toNum "eightwothree"

printfn $"{digs1}" 
printfn $"{digs}" 
