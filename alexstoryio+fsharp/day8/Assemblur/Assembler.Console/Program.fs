open System.IO

type Outcome = Fail | Succeed
let (|Acc|Jmp|Nop|) (str:string) =
    match str.Contains("jmp"), str.Contains("acc") with
    | true, _ -> Jmp
    | _, true -> Acc
    | _,_ -> Nop

let (|Add|Sub|) (str: string) =
    match str.Contains("+") with
    | true  -> Add (str.Split("+").[1] |> int)
    | false -> Sub (str.Split("-").[1] |> int)


let content =
    __SOURCE_DIRECTORY__ + "/input.txt"
    |> File.ReadAllLines

let flip str =
    match str with
    | Jmp -> str.Replace("jmp", "nop")
    | Acc -> str.Replace("nop", "jmp")
    | Nop -> str

let rec parse (instructions: string []) acc idx history =
    if Seq.contains idx history then
        acc, Fail
    elif idx = Seq.length instructions then
        acc, Succeed
    elif idx > Seq.length instructions then
        acc, Fail
    else 
        let str = instructions.[idx]
        match str with
        | Acc -> match str with 
                 | Add x -> parse instructions (acc + x) (idx + 1) (idx::history)
                 | Sub x -> parse instructions (acc - x) (idx + 1) (idx::history)
        | Jmp -> match str with
                 | Add x -> parse instructions acc (idx + x) (idx::history)
                 | Sub x -> parse instructions acc (idx - x) (idx::history)
        | Nop -> parse instructions acc (idx + 1) (idx::history)

let parse' instructions = parse instructions 0 0 []

for idx, _item in Seq.indexed content do
    let newInstructions = Seq.mapi (fun i t -> if i = idx then flip t else t) content
    let result = parse' <| Seq.toArray newInstructions
    if snd result = Succeed then
        printfn "%A - line %d" result idx

[<EntryPoint>]
let main _argv =
    // parse' content
    // |> printfn "%A" 
    0 // return an integer exit code