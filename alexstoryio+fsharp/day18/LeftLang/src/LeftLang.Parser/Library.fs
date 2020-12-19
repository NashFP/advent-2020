namespace LeftLang

open Tokenizer
open LeftLang.Core
open LeftLang.Core.Types

module Parser = 
    let private commands =
        [
            (Addition, Operator.addition)
            (Subtraction, Operator.subtraction)
            (Multiplication, Operator.multiplication)
            (Division, Operator.division)
        ]
        |> Map.ofList

    let rec private parse' (tokens: Token list) (command: Operator option) (left: Number option) (tlength: int) =
        match List.length tokens, command, left with
        | 0, Some(_), _    -> failwith "Incomplete expression"
        | 0, None, None    -> failwith "Empty argument"
        | 0, None, Some(n) -> n, tlength
        | _, _, _          -> 
            match left, command, List.head tokens with
            | _, _, Newline              -> parse' (List.tail tokens) command left (tlength + 1)
            | Some _, Some _, CloseParen -> failwith "Unexpected )"
            | Some l, None, CloseParen   -> l, tlength 
            | None, None, Number n       -> parse' (List.tail tokens) None (Some n) (tlength + 1)
            | Some l, None, Number n     -> 
                printfn "left: %A, right: %A" l n
                failwith "invalid arguments"
            | Some _, None, Operator o   -> parse' (List.tail tokens) (Some o) left (tlength + 1)
            | Some l, Some o, Number r   -> 
                match o with
                | Addition -> 
                    let command = Map.find o commands
                    let result = command l r
                    parse' (List.tail tokens) None (Some result) (tlength + 1)
                | Multiplication ->
                    let command = Map.find o commands 
                    let rhs = parse' tokens None None 0
                    let result = command l (fst rhs)
                    parse' [] None (Some result) (tlength + (snd rhs))
                | _ -> failwith "not there yet chief"
            | _, _, OpenParen            ->
                let tail = List.tail tokens
                let result, skipLength = parse' tail None None 0
                let tail = List.skip (skipLength + 1) tail
                let nResult = Tokenizer.Number result
                parse' (nResult :: tail) command left (tlength + skipLength + 1)
            | l, o, r -> failwith  (sprintf "Invalid state: left-%A op-%A right%A" l o r)

    let rec parse (tokens: Token list) =
        parse' tokens None None 0
        |> fst