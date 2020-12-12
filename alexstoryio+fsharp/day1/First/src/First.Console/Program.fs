open System.IO

let findMate lst x = 
    Seq.tryFind (fun item -> x + item = 2020) lst


let appendSome (arr: seq<'a>) (item: 'a option) =
    match item with
    | None -> arr 
    | Some(x) -> Seq.append arr (Seq.singleton x)

let results = 
    __SOURCE_DIRECTORY__ + "/input.txt"
    |> File.ReadAllLines
    |> Seq.map int
    |> fun numbers -> Seq.map (findMate numbers) numbers
    |> Seq.fold appendSome Seq.empty
    |> Seq.toList
    |> fun [x; y] -> x * y


[<EntryPoint>]
let main argv =
    results
    |> printf "%A\n"
    0 // return an integer exit code