// Program.fs
// Seatway
// aOc Day 10 - 2020
// Purposely did this one in a more enterprise like way

open System.IO

type Seat = Empty | Full | Floor

let seat = function
    | '.' -> Floor
    | 'L' -> Empty
    | '#' -> Full
    | _   -> failwith "Invalid seat"

type Spot = { 
    Seat: Seat
    X: int
    Y: int
}

type Board = Seat [][]

module Board =
    let x spot = spot.X
    let y spot = spot.Y
    let seat spot = spot.Seat

    let maxX (board: Board) =
        board 
        |> Seq.head
        |> Seq.length
        |> fun x -> x - 1
    
    let maxY board =
        board
        |> Seq.length
        |> fun x -> x - 1

    let get (board: Board) x y =
        board.[y].[x]


    let up board spot =
        if y spot = 0 then
            true
        else
            match get board spot.X (spot.Y-1) with
            | Full -> false
            | _    -> true
    
    let rec up' board x y =
        if y = 0 then
            true
        else 
            match get board x (y-1) with
            | Full  -> false
            | Empty -> true
            | Floor -> up' board  x (y-1)

    let left board spot = 
        if x spot = 0 then
            true
        else 
            match get board (spot.X-1) spot.Y with
            | Full -> false
            | _    -> true

    let rec left' board x y =
        if x = 0 then 
            true
        else 
            match get board (x-1) y with
            | Full  -> false
            | Empty -> true
            | Floor -> left' board (x-1) y

    let down board spot =
        if y spot = maxY board then
            true
        else 
            match get board spot.X (spot.Y + 1) with
            | Full -> false
            | _    -> true

    let rec down' board x y =
        if y = maxY board then
            true
        else 
            match get board x (y+1) with
            | Full  -> false
            | Empty -> true
            | Floor -> down' board x (y+1)

    let right board spot =
        if x spot = maxX board then
            true
        else 
            match get board (spot.X + 1) spot.Y with
            | Full -> false
            | _    -> true

    let rec right' board x y =
        if x = maxX board then
            true
        else 
            match get board (x+1) y with
            | Full  -> false
            | Empty -> true
            | Floor -> right' board (x+1) y

    let upLeft board spot = 
        if x spot = 0 || y spot = 0 then
            true
        else 
            match get board (spot.X-1) (spot.Y-1) with
            | Full -> false
            | _    -> true

    let rec upLeft' board x y =
        if x = 0 || y = 0 then
            true
        else 
            match get board (x-1) (y-1) with
            | Full -> false
            | Empty -> true
            | Floor -> upLeft' board (x-1) (y-1)

    let upRight board spot = 
        if x spot = maxX board || y spot = 0 then
            true
        else 
            match get board (spot.X+1) (spot.Y-1)  with
            | Full -> false
            | _    -> true

    let rec upRight' board x y =
        if x = maxX board || y = 0 then
            true
        else
            match get board (x+1) (y-1) with
            | Full  -> false
            | Empty -> true
            | Floor -> upRight' board (x+1) (y-1)

    let downLeft board spot =
        if x spot = 0 || y spot = maxY board then
            true
        else 
            match get board (spot.X-1) (spot.Y+1) with
            | Full -> false
            | _    -> true

    let rec downLeft' board x y = 
        if x = 0 || y = maxY board then
            true
        else 
            match get board (x-1) (y+1) with
            | Full  -> false
            | Empty -> true
            | Floor -> downLeft' board (x-1) (y+1)

    let downRight board spot =
        if x spot = maxX board || y spot = maxY board then
            true
        else 
            match get board (spot.X+1) (spot.Y+1) with
            | Full -> false
            | _    -> true

    let rec downRight' board x y =
        if x = maxX board || y = maxY board then
            true
        else 
            match get board (x+1) (y+1) with
            | Full  -> false
            | Empty -> true
            | Floor -> downRight' board (x+1) (y+1)

    let neighborCount board spot =
        [
            up board spot
            down board spot
            left board spot
            right board spot
            upLeft board spot
            upRight board spot
            downLeft board spot
            downRight board spot
        ] 
        |> List.filter not
        |> List.length

    let neighborCount' board spot = 
        [
            up' board spot.X spot.Y
            down' board spot.X spot.Y
            left' board spot.X spot.Y
            right' board spot.X spot.Y
            upLeft' board spot.X spot.Y
            upRight' board spot.X spot.Y
            downLeft' board spot.X spot.Y
            downRight' board spot.X spot.Y
        ]
        |> List.filter not
        |> List.length

    let getNext board spot =
        match spot.Seat with
        | Floor                                   -> Floor
        | Empty when neighborCount board spot = 0 -> Full
        | Empty                                   -> Empty
        | Full when neighborCount board spot >= 4 -> Empty
        | Full                                    -> Full
        
    let getNext' board spot =
        match spot.Seat with
        | Floor                                    -> Floor
        | Empty when neighborCount' board spot = 0 -> Full
        | Empty                                    -> Empty
        | Full when neighborCount' board spot >= 5 -> Empty
        | Full                                     -> Full

    let step (board: Board): Board =
        [|
            for y, row in Array.indexed board do
                [|
                    for x, value in Array.indexed row do
                        getNext board { X = x; Y = y; Seat = value }
                |]
        |]

    let step' (board: Board)  : Board = 
        [|
            for y, row in Array.indexed board do
                [|
                    for x, value in Array.indexed row do
                        getNext' board { X = x ; Y = y ; Seat = value  }
                |]
        |]

    let fullCount board =
        [
            for y in board do
                for x in y do
                    if x = Full then 1 else 0
        ]
        |> Seq.sum


let content: Board = 
    __SOURCE_DIRECTORY__ + "/input.txt"
    |> File.ReadAllLines
    |> Array.map(fun x -> x.ToCharArray() |> Array.map seat)


let rec loop board last iter=
    if board = last then
        board
    else 
        loop (Board.step board) board (iter+1)

let rec loop' board last iter =
    printfn "full: %d iter: %d" (Board.fullCount board) iter
    if board = last then
        board
    else
        loop' (Board.step' board) board (iter+1)



[<EntryPoint>]
let main argv =
    loop' content [||] 1
    |> Board.fullCount
    |> printfn "%A"
    0 // return an integer exit code