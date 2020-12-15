open System.IO
open FSharpPlus
open FSharp.Collections
open System.Collections.Generic

let content = 
        let mutable dict = new Dictionary<int, int> ()
        let contents = 
            __SOURCE_DIRECTORY__ + "/input.txt"
            |> File.ReadAllText
            |> String.split [","]
            |> map int 
            |> Seq.indexed
            |> map (fun (i, v) -> v, (i+1))
        
        for (v, i) in contents do
            dict.[v] <- i

        dict

let getVal value turn (game: Dictionary<int, int>) =
    match game.ContainsKey(value) with
    | false -> 0
    | true  -> 
        let last =  game.[value]
        turn - last

let rec play game goal value turn =
    let newValue = getVal value turn game

    if turn = goal then
        value
    else
        game.[value] <- turn
        play game goal newValue (turn + 1)

let partOne () =
    play content 2020 0 (length content |> (+) 1)
    |> printfn "part 1: %d"

let partTwo () =
    play content 30_000_000 0 (length content |> (+) 1)
    |> printfn "part 2: %A" 
    
[<EntryPoint>]
let main _argv =
    partOne()
    partTwo()
    0