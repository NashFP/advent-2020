open System.IO

let content =
    __SOURCE_DIRECTORY__ + "/input.txt"
    |> File.ReadAllText
    |> fun x -> x.Split "\n\n"

let hasLetter (ch: char) (str: string) =
    Seq.contains ch (str.ToCharArray())

let allHaveLetter ch strings =
    Seq.forall (hasLetter ch) strings

let splitLines (x: string) = x.Split "\n"

let getAllReperesented strings =
    let first: string = Seq.head strings
    let rest = Seq.tail strings
    if Seq.isEmpty rest then
        Array.toSeq (first.ToCharArray())
    else 
        Seq.filter (fun x -> allHaveLetter x rest) first

let removeNewlines (str: string) = str.Replace("\n", "")

let path2 = 
    content
    |> Seq.map splitLines
    |> Seq.map getAllReperesented
    |> Seq.map Seq.length
    |> Seq.sum
    //Seq.sumBy (splitLines >> getAllReperesented >> Seq.length) content

let path1 =
    content
    |> Seq.sumBy (removeNewlines >> Seq.distinct >> Seq.length)


[<EntryPoint>]
let main argv =
    path2
    |> printfn "%A"
    0 // return an integer exit code