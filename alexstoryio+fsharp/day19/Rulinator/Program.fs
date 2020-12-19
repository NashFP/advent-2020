open System
open System.IO
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions
open FSharp.Collections.ParallelSeq

type RuleRegex = Regex< @"(?<Number>\d+):\s(?<Rule>.+)">
type LetterRegex = Regex< "\"(?<Letter>\w)\"">
type OptionRegex = Regex<"(?<Left>.+)\|(?<Right>.+)">
type BinaryRegex = Regex<"(?<Left>\d+)\s+(?<Right>\d+)">
type TrinaryRegex = Regex<"(?<Left>\d+)\s+(?<Center>\d+)\s+(?<Right>\d+)">
type ReferenceRegex = Regex<"\s*(?<Number>\d+)\s*">

let (|Letter|Binary|Option|Reference|Trinary|) rule =
    if LetterRegex().IsMatch rule then
        Letter rule
    elif OptionRegex().IsMatch rule then
        Option rule
    elif TrinaryRegex().IsMatch rule then
        Trinary rule
    elif BinaryRegex().IsMatch rule then
        Binary rule
    elif ReferenceRegex().IsMatch rule then
        Reference rule
    else 
        printfn "RULE: %A" rule
        failwith "not there yet"

let extractRule rule =
    let m = RuleRegex().TypedMatch rule
    (m.Number.AsInt, m.Rule.Value)

let parseRules rules =
    rules
    |> Array.map extractRule
    |> Map.ofArray

let rec letterMatch rule (text: string[]) = 
    let text = 
        text
        |> Array.filter (String.IsNullOrEmpty >> not)
        |> Array.map (fun s ->
            let  m = LetterRegex().TypedMatch(rule).Letter.AsChar
            m = s.[0], s.[1..])
        |> Array.filter (fst)
        |> Array.map snd
    if Array.isEmpty text then
        false, [||]
    else
        true, text


and binaryMatch rules rule text =
    let m = BinaryRegex().TypedMatch rule
    let left = m.Left.AsInt
    let right = m.Right.AsInt
    let leftRule = Map.find left rules
    let rightRule = Map.find right rules

    match matches rules leftRule text with
    | false, _        -> false, text
    | true, remaining ->
        matches rules rightRule remaining

and trinaryMatch rules rule text =
    let m = TrinaryRegex().TypedMatch rule
    let left = m.Left.AsInt
    let center = m.Center.AsInt
    let right = m.Right.AsInt

    let leftRule = Map.find left rules
    let centerRule = Map.find center rules
    let rightRule = Map.find right rules

    match matches rules leftRule text with
    | false, _ -> false, text
    | true, remaining ->
        match matches rules centerRule remaining with
        | false, _  -> false, text
        | true, rem -> matches rules rightRule rem

and optionMatch rules rule text =
    let m = OptionRegex().TypedMatch rule
    let left = m.Left.Value
    let right = m.Right.Value

    match matches rules left text, matches rules right text with
    | (false, _), (false, _) -> false, text
    | (true, lr), (true, rr) -> true, (Array.append lr rr)
    | (true, lr), _          -> true, lr
    | _, (true, rr)          -> true, rr

and referenceMatch rules rule text =
    let m = ReferenceRegex().TypedMatch rule
    let number = m.Number.AsInt
    let r = Map.find number rules
    matches rules r text

and matches rules rule (text: string []) =
    match rule with
    | Letter r    -> letterMatch r text
    | Option r    -> optionMatch rules r text
    | Trinary r   -> trinaryMatch rules r text
    | Binary r    -> binaryMatch rules r text
    | Reference r -> referenceMatch rules r text

let content = 
    __SOURCE_DIRECTORY__ + "/input.txt"
    |> File.ReadAllLines
    |> Array.partition (RuleRegex().IsMatch)

let rules = 
    content
    |> fst
    |> parseRules

let messages = 
    content
    |> snd
    |> Array.tail

let zero = Map.find 0 rules
let matchesZero = matches rules zero

messages
|> PSeq.map (Array.singleton >> matchesZero)
|> PSeq.filter fst
|> PSeq.filter (snd >> (Array.exists String.IsNullOrEmpty))
|> PSeq.length
|> printfn "%d"