namespace Second

open Utils

type Password = {
    Original: string
    Password: string
    Code: string
    First: int
    Second: int
    Letter: char
}
    with
        member x.Raw = Array.filter (fun c -> c = x.Letter) (x.Password.ToCharArray())
        member x.LetterCount = Array.length x.Raw
        member x.Valid: bool = xor (x.Password.[x.First - 1] = x.Letter)  (x.Password.[x.Second - 1] = x.Letter)
        
        static member Default = 
            {
                Original = ""
                Password = ""
                Code = ""
                First = 0
                Second = 0
                Letter = ' '
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
               First = int (password.Code.Split("-").[0].Trim())
            }
        static member ExtractMax password =
            { password with
                Second = int (password.Code.Split("-").[1].Split(" ").[0])
            }
        static member ExtractLetter password =
            { password with
                Letter = password.Code.Split("-").[1].Split(" ").[1].[0]
            }