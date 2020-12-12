open System.IO

let rows = [0..127]
let cols = [0..7]
let tickets = [0..866]
type Row = F | B
type Seat = L | R

let parseRow = function
    | 'F' -> F
    | 'B' -> B
    | _ -> failwith "Invalid row"

let parseSeat = function
    | 'L' | 'F' -> L
    | 'R' | 'B' -> R
    | _ -> failwith "Invalid seat"

let partRow dir seats =
    let pivot = List.length seats / 2
    match dir with
    | F -> seats.[..pivot]
    | B -> seats.[pivot..]

let partCol dir seats = 
    let pivot = List.length seats / 2
    match dir with
    | L -> seats.[..pivot]
    | R -> seats.[pivot..]

type Ticket = {
    Number: string
    RowPart: Row[]
    ColPart: Seat[]
    SeatRow: int
    SeatCol: int
}

let rec parseRowSeq seq seats =
    match seq with
    | [] -> List.head seats
    | x :: xs -> parseRowSeq xs (partRow x seats)


let rec parseColSeq seq seats =
    match seq with
    | [] -> List.head seats
    | x::xs -> parseColSeq xs (partCol x seats)

module Ticket = 
    let empty = {
        Number = ""
        RowPart = [||]
        ColPart = [||]
        SeatRow = 0
        SeatCol = 0
    }

    let parseRowPart ticket =
        let rowpart = 
            ticket.Number.[..6].ToCharArray()
            |> Array.map parseRow
        { ticket with RowPart = rowpart}
    
    let parseSeatPart ticket = 
        let seatpart = 
            ticket.Number.[7..].ToCharArray()
            |> Array.map parseSeat
        { ticket with ColPart = seatpart }

    let parseRow ticket =
        let seat = parseRowSeq (Array.toList ticket.RowPart) rows
        { ticket with  SeatRow = seat}

    let parseCol ticket =
        let seat = parseColSeq (Array.toList ticket.ColPart) cols
        { ticket with SeatCol = seat}

    let seatId ticket =
        (ticket.SeatRow * 8) + ticket.SeatCol

    let create number = 
        { empty with Number = number}
        |> parseRowPart
        |> parseSeatPart
        |> parseRow
        |> parseCol


let content = 
    __SOURCE_DIRECTORY__ + "/input.txt"
    |>File.ReadAllLines 
    |> Array.map (Ticket.create >> Ticket.seatId)

let notFound n =
     not (Array.contains n content)
     && (Array.contains (n-1) content)
     && (Array.contains (n+1) content)

let ticketNum = 
    tickets
    |> Seq.filter notFound 

[<EntryPoint>]
let main argv =
    ticketNum
    |> printfn "%A"
    0 // return an integer exit code