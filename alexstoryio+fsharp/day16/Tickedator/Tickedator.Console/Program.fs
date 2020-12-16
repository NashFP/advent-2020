open System.IO
open FSharpPlus
open FSharpPlus.Lens
open FSharp.Text.RegexProvider

type RuleRegex = Regex< @"(?<RuleName>.*):\s(?<LHMin>\d*)-(?<LHMax>\d*)\sor\s(?<RHMin>\d*)-(?<RHMax>\d*)" >

type Rule =
    {
        Name: string
        LeftMin: int
        LeftMax: int
        RightMin: int
        RightMax: int
    }

module Rule =   
    let parse text =
        let matched = RuleRegex().TypedMatch(text)
        {
            Name = matched.RuleName.Value
            LeftMin = int matched.LHMin.Value
            LeftMax = int matched.LHMax.Value
            RightMin = int matched.RHMin.Value
            RightMax = int matched.RHMax.Value
        }

    let passes num rule =
        (num >= rule.LeftMin && num <= rule.LeftMax)
        || (num >= rule.RightMin && num <= rule.RightMax)

    let passesAny rules num = exists (passes num) rules
    let allPass nums rule = forall (flip passes rule) nums

let rec getCandidates' rules tickets candidates idx =
    if idx = length (nth 0 tickets) then
        candidates
    else 
        let currentNumbers = map (nth idx) tickets
        let activeRules = filter (Rule.allPass currentNumbers) rules
        let newMap = Map.map (fun key value -> if Array.contains key activeRules then idx::value else value ) candidates
        getCandidates' rules tickets newMap (idx + 1)

let getCandidates rules tickets =
    let initial = 
        rules
        |> map (fun r -> (r, List.empty))
        |> Map.ofArray

    getCandidates' rules tickets initial 0

let rec removeOthers rule num candidates =
    Map.map (fun key value -> if key = rule then value else List.except [num] value) candidates

let rec trimCandidates candidates = 
    let singles = Map.filter(fun key value -> length value = 1) candidates

    if length singles = length candidates then
        candidates
    else 
        let newMap = Map.fold (fun acc key value -> removeOthers key (head value) acc) candidates singles
        trimCandidates newMap


let content = 
    let rawContent = 
        __SOURCE_DIRECTORY__ + "/input.txt"
        |> File.ReadAllLines
        |> split [[|""|]]

    let rules = map Rule.parse rawContent.[0]
    let ticket = map int (split [|","|] rawContent.[1].[1])
    let otherTicketsRaw = skip 1 rawContent.[2]
    let otherTickets = map (split [|","|] >> map int) otherTicketsRaw
        
    (rules, ticket, otherTickets)

let partOne () =
    content
    |> view _3
    |> Array.concat
    |> filter ((Rule.passesAny (view _1 content)) >> not)
    |> sum
    |> printfn "%A"

let partTwo () =
    let (rules, ticket, otherTickets) = content
    let validTickets = filter (forall (Rule.passesAny rules)) otherTickets

    Array.append validTickets [|ticket|]
    |> getCandidates rules
    |> trimCandidates 
    |> Map.filter (fun key value -> String.isSubString "departure" key.Name) 
    |> Map.values 
    |> Seq.map head
    |> map (fun i -> ticket.[i])
    |> map (fun (n: int) -> bigint n)
    |> Seq.reduce ( * ) 
    |> printfn "%A"

[<EntryPoint>]
let main argv =
    partOne()
    partTwo()
    0 // return an integer exit code