open System.IO


let getJolt (joltage, ones, threes) adapter =
    if joltage + 1 = adapter then
        (adapter, ones + 1, threes)
    elif joltage + 3 = adapter then
        (adapter, ones, threes + 1)
    else 
        (adapter, ones, threes)

let content = 
    __SOURCE_DIRECTORY__ + "/input.txt"
    |> File.ReadAllLines
    |> Seq.map int
    |> Seq.sort

let differences = 
    content
    |> Seq.fold getJolt (0, 0, 0)

let canReach original target = 
    target > original 
    && target - original <= 3


let rec pathCount lst =
    printfn "%A" lst
    match lst with
    | [_, count] -> count
    | (hn, hc) :: tail ->
        pathCount (List.map (fun (n, c) -> (n, if canReach hn n then hc + c else c)) tail)



// let part1 = 
//     differences
//     |> fun (a, b, c) -> printfn "joltages: %d, ones: %d, threes: %d, answer: %d" a b c (b * (c + 1))


[<EntryPoint>]
let main argv =
    let counts = (0, bigint 1) :: List.map (fun x -> x, bigint 0) (Seq.toList content)
    counts
    |> pathCount
    |> printfn "%A"
    0 // return an integer exit code