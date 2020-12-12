open System.IO

type Password = {
    Original: string
    Password: string
    Code: string
    Min: int
    Max: int
    Letter: string
}
    with
        member x.Raw = Array.filter (fun c -> c = x.Letter.[0]) (x.Password.ToCharArray())
        member x.LetterCount = Array.length x.Raw
        member x.Valid = x.LetterCount >= x.Min && x.LetterCount <= x.Max
        
        static member Default = 
            {
                Original = ""
                Password = ""
                Code = ""
                Min = 0
                Max = 0
                Letter = ""
            }

        static member Init init = 
            init
            |> fun x -> { Password.Default with Original = x}
            |> Password.ExtractPassword
            |> Password.ExtractCode
            |> Password.ExtractMin
            |> Password.ExtractMax
            |> Password.ExtractLetter

        static member ExtractPassword password =
            { password with 
                Password = password.Original.Split(":").[1].Trim() 
            }
        static member ExtractCode password =
            { password with
                Code = password.Original.Split(":").[0].Trim()
            }
        static member ExtractMin password = 
            { password with
               Min = int (password.Code.Split("-").[0].Trim())
            }
        static member ExtractMax password =
            { password with
                Max = int (password.Code.Split("-").[1].Split(" ").[0])
            }
        static member ExtractLetter password =
            { password with
                Letter = password.Code.Split("-").[1].Split(" ").[1]
            }


let content =
    __SOURCE_DIRECTORY__ + "/input.txt"
    |> File.ReadAllLines
    |> Array.map(Password.Init)
    |> Array.filter(fun x -> x.Valid)


[<EntryPoint>]
let main argv =
    content
    |> Array.length
    |> printfn "%A" 
    0 // return an integer exit code