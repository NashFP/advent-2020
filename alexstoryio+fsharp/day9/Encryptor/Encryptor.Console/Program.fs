open System.IO

let getCombinations (list: seq<bigint>) =
    [
        for x in list do
            for y in list do
                if x <> y then
                    x + y
    ]


let content = 
    __SOURCE_DIRECTORY__ + "/input.txt"
    |> File.ReadAllLines
    |> Seq.map bigint.Parse


let subset = content |> Seq.skip 25

let possibilities i = 
    content 
        |> Seq.skip (i) 
        |> (Seq.take 25)
        |> getCombinations 

let isValid i v = 
    let possibles = possibilities i
    Seq.contains v possibles


let rec findSum n list=
    if Seq.isEmpty list then 
        (0, 0)
    else 
        let result  = 
            list
            |> Seq.scan (fun acc (i , num) -> (i, num + snd acc)) (0, bigint 0)
        match Seq.contains n (Seq.tail result |> Seq.map snd) with
        | true -> 
            let start = Seq.head list |> fst
            let stop = Seq.find (fun x -> snd x = n) result |> fst
            (start, stop)
        | false -> findSum n (Seq.tail list)

let summate n list = 
    findSum n (Seq.indexed list)

subset
    |> Seq.indexed
    |> Seq.filter (fun (i, x) -> not (isValid i x))
    |> Seq.iter (printfn "%A")




[<EntryPoint>]
let main argv =
    let n = bigint 217430975
    summate n content
    |> fun (start, stop) -> Seq.toArray(content).[start..stop]
    |> fun lst -> (Seq.min(lst)) + (Seq.max(lst))
    // "alex"
    |> printfn "%A"
    0 // return an integer exit code