namespace LeftLang

open System
open System.Text.RegularExpressions
open LeftLang.Core


module Tokenizer =
    type Operator =
        Addition
        | Subtraction
        | Division
        | Multiplication

    type Token =
        Operator of Operator
        | Number of Types.Number
        | OpenParen
        | CloseParen
        | Newline


    let isNumeric (text: string): bool =
        match Int32.TryParse text with
        | true, _ -> true
        | _, _    -> false
   

    let parseToken = function
    | "+"                -> Operator Addition
    | "/"                -> Operator Division
    | "*"                -> Operator Multiplication
    | "\n"               -> Newline
    | "("                -> OpenParen
    | ")"                -> CloseParen
    | n when isNumeric n -> n |> int64 |> Number
    | t                  -> failwith (sprintf "Invalid token: %s" t)

    let tokenize (text: string) =
        let text = 
            text
                .Replace(Environment.NewLine, "\n")
                .Replace("(", " ( ")
                .Replace(")", " ) ")

        let text =
            Regex.Replace(text, "\s+", " ")

        let text = 
            Regex.Replace(text, "#.+", "")

        Array.map(parseToken) (text.Trim().Split(" "))
        |> Array.toList
