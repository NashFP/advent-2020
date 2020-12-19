open System
open System.IO
open LeftLang


let processString (text: string) =
    text
    |> Tokenizer.tokenize 
    |> Parser.parse

let run' input args =
    let input = File.ReadAllLines input
    let results = Array.map processString input

    if Array.contains ("--sum") args then
        results
        |> Array.sum
        |> printfn "%d"
    else
        results
        |> Array.iter (printfn "%d")
        

let run input =
    let input = File.ReadAllText input

    
    try 
        let result = processString input
        
        printf "%A" result
        //Seq.iter(printfn "%A") tokens
    with
        | Failure(msg) -> 
            printfn "%s" msg
            Environment.Exit 1

        
    


[<EntryPoint>]
let main argv =
    match argv.Length with
    | 0 -> printfn "no arguments given expected file"
    | 1 -> run' argv.[0] [|"--all"|]
    | _ -> run' argv.[0] argv.[1..]
    0 