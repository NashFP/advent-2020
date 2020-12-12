open System.IO
open Microsoft.FSharp.Linq.NullableOperators

type Passport = {
    RawString: string
    BirthYear: string option
    IssueYear: string option
    ExpirationYear: string option
    Height: string option
    HairColor: string option
    EyeColor: string option
    PassportId: string option
    CountryId: string option
}

module Passport =
    let Default = {
            RawString = ""
            BirthYear = None
            IssueYear = None
            ExpirationYear = None
            Height = None
            HairColor = None
            EyeColor = None
            PassportId = None
            CountryId = None
    } 
    let ParseCountryId passport = 
        let countryId =
            passport.RawString.Split(" ")
            |> Array.tryFind(fun x -> x.StartsWith("cid:"))
            |> Option.map(fun x -> x.Remove(0, 4))
        { passport with CountryId = countryId }

    let ParsePassportId passport = 
        let passportId =
            passport.RawString.Split(" ")
            |> Array.tryFind(fun x -> x.StartsWith("pid:"))
            |> Option.map(fun x -> x.Remove(0, 4))
        { passport with PassportId = passportId }

    let ParseEyeColor passport = 
        let eyeColor =
            passport.RawString.Split(" ")
            |> Array.tryFind(fun x -> x.StartsWith("ecl:"))
            |> Option.map(fun x -> x.Remove(0, 4))
        { passport with EyeColor = eyeColor }

    let ParseHairColor passport = 
        let hairColor =
            passport.RawString.Split(" ")
            |> Array.tryFind(fun x -> x.StartsWith("hcl:"))
            |> Option.map(fun x -> x.Remove(0, 4))
        { passport with HairColor = hairColor }

    let ParseHeight passport = 
        let height =
            passport.RawString.Split(" ")
            |> Array.tryFind(fun x -> x.StartsWith("hgt:"))
            |> Option.map(fun x -> x.Remove(0, 4))
        { passport with Height = height }

    let ParseExpirationYear passport = 
        let expirationYear =
            passport.RawString.Split(" ")
            |> Array.tryFind(fun x -> x.StartsWith("eyr:"))
            |> Option.map(fun x -> x.Remove(0, 4))
        { passport with ExpirationYear = expirationYear }

    let ParseIssueYear passport =
        let issueYear = 
            passport.RawString.Split(" ")
            |> Array.tryFind(fun x -> x.StartsWith("iyr:"))
            |> Option.map(fun x -> x.Remove(0, 4))
        { passport with IssueYear = issueYear}

    let parseBirthYear passport =
        let birthYear = 
            passport.RawString.Split(" ")
            |> Array.tryFind(fun x -> x.StartsWith("byr:"))
            |> Option.map(fun x -> x.Remove(0, 4))
        { passport with BirthYear = birthYear}

    let create init = 
        { Default with RawString = init }
        |> parseBirthYear
        |> ParseIssueYear
        |> ParseExpirationYear
        |> ParseHeight
        |> ParseHairColor
        |> ParseEyeColor
        |> ParsePassportId
        |> ParseCountryId

    let validateBirthYear passport =
        let testYear x = x >= 1920 && x <= 2002
        
        match passport.BirthYear with
        | None -> false
        | Some(year) ->  year |> int |> testYear
    

    let validateIssueYear passport =
        let testYear x = x >= 2010 && x <= 2020
        match passport.IssueYear with
        | None -> false
        | Some(year) -> year |> int |> testYear

    let validateExpirationYear passport =
        let testYear x = x >= 2020 && x <= 2030
        match passport.ExpirationYear with
        | None -> false
        | Some(year) -> year |> int |> testYear

    let validateHeight passport =
        let testHeight (ht: string) =
            match ht.Contains("cm"), ht.Contains("in") with
            | true, _ -> ht.Replace("cm", "") |> int |> fun x -> x >= 150 && x <= 193
            | _, true -> ht.Replace("in", "") |> int |> fun x -> x >= 59 && x <= 76
            | _, _ -> false
        
        match passport.Height with
        | None -> false
        | Some(height) -> testHeight height

    let validateHairColor passport =
        let testColor color =
            String.length color = 7
            && color.[0] = '#'
            && Array.length (Array.filter (fun x -> (Array.contains x [|'0'..'9'|]) || (Array.contains x [|'a'..'f'|])) (color.[1..].ToCharArray())) = 6

        match passport.HairColor with
        | None -> false
        | Some(color) -> testColor color

    let validateEyeColor passport =
        match passport.EyeColor with
        | None -> false
        | Some(color) -> List.contains color ["brn"; "blu"; "amb"; "gry"; "grn"; "hzl"; "oth"] 

    let validatePassportId passport =
        let testNumber n =
            String.length n = 9
            && String.length (String.filter(fun x -> List.contains x ['0'..'9']) n) = 9
        
        match passport.PassportId with
        | None -> false
        | Some(number) -> testNumber number 

    let isValid passport =
        validateBirthYear passport
        && validateIssueYear passport
        && validateExpirationYear passport
        && validateHeight passport
        && validateHairColor passport
        && validateEyeColor passport
        && validatePassportId passport

let content = 
    __SOURCE_DIRECTORY__ + "/input.txt"
    |> File.ReadAllText
    |> fun x -> x.Split("\n\n")
    |> Array.map(fun x -> Passport.create (x.Replace("\n", " ")))
    |> Array.filter Passport.isValid

[<EntryPoint>]
let main argv =
    content
    |> Array.length
    |> printfn "%A" 
    0 // return an integer exit code