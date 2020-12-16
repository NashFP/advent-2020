open System
open System.IO
open FSharpPlus
open FSharp.Text.RegexProvider

type MaskRegex = Regex< @"(?<=mask\s=\s)(?<Mask>.*)" >
type MemoryRegex = Regex< @"(?<=mem\[)(?<Index>\d+)\]\s=\s(?<Val>\d+)" >

let update idx v list = Array.mapi (fun i x -> if i = idx then v else x) list
let updateString idx v str = 
    str 
    |> String.toArray
    |> update idx v
    |> String.ofArray

let tupdate idx v list =
    match tryFind (fst >> (=) idx) list with
    | None -> Array.append list [| (idx, v) |]
    | Some(item) -> Array.replace [| item |] [|(idx, v)|] list

let (|Mask|Mem|) str = 
    match MaskRegex().IsMatch str with
    | true -> Mask str
    | false -> Mem str

let flatten xs = [|for x in xs do for y in x -> y|] 

let parseMask line = MaskRegex().TypedMatch(line).Mask.Value

let parseMemory line =
    let idx = int (MemoryRegex().TypedMatch(line).Index.Value)
    let value = int (MemoryRegex().TypedMatch(line).Val.Value)
    idx, value

let num2Bin (num: int) len = Convert.ToString (num, 2) |> String.padLeftWith len '0'
let bin2Num num = Convert.ToInt64(num, 2)

let merge mask number =
    let num = num2Bin number (length mask)

    Seq.map2 (fun m o -> if m = 'X' then o else m) mask num
    |> String.ofSeq
    |> bin2Num

let rec getVariants (codes: string[]) = 
    match String.tryFindIndex ((=) 'X') (head codes) with
    | None    -> codes
    | Some(n) -> 
        map (fun x -> [| updateString n '1' x ; updateString n '0' x |]) codes
        |> flatten
        |> getVariants

let decode mask id =
    let address = num2Bin id (length mask)
    let initialString = Seq.map2 (fun m o -> if m = '0' then o else m) mask address |> String.ofSeq
    
    [|initialString|]
    |> getVariants
    |> map bin2Num

let rec runCommands commands memory mask =
    match commands with
    | [||] -> memory 
    | _  -> 
        match head commands with 
        | Mask x -> runCommands (skip 1 commands) memory (parseMask x) 
        | Mem x -> 
            let (idx, value) = parseMemory x

            if idx < (length memory) then
                runCommands (skip 1 commands) (update idx (merge mask value) memory) mask
            else
                failwith "oops"

let rec runDecoder commands memory mask =
    match commands with
    | [||] -> memory
    | _ ->
        match head commands with
        | Mask x -> runDecoder (skip 1 commands) memory (parseMask x)
        | Mem x ->
            let (idx, value) = parseMemory x

            let m = 
                decode mask idx
                |> fold (fun mem idx -> tupdate idx (bigint value) mem) memory
            runDecoder (skip 1 commands) m mask

let content () =
    __SOURCE_DIRECTORY__ + "/input.txt"
    |> File.ReadAllLines

let part1 () =
    let commands = content ()
    let memory = Array.init 100_000 (konst 0L)
    runCommands commands memory ""
    |> sum

let part2() =
    let commands = content()
    let memory = [||]

    runDecoder commands memory ""
    |> map snd 
    |> sum

[<EntryPoint>]
let main argv =
    match tryHead argv with
    | None      -> printfn "Enter 1 to run part 1 or 2 to run part 2"
    | Some("1") -> part1 () |> printfn "%A"
    | Some("2") -> part2 () |> printfn "%A"
    | _         -> ()
    0 // return an integer exit code