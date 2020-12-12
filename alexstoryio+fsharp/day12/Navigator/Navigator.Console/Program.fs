open System.IO

let extractDigit (str: string) letter = 
    str.Replace(letter, "")
    |> fun x -> x.Trim()
    |> int

type Heading = North | East | South | West

let turnLeft = function
    | East   -> North
    | South  -> East
    | West   -> South
    | North  -> West

let turnRight = function
    | East  -> South
    | South -> West
    | West  -> North
    | North -> East

type Turn = Left | Right

let (|L|R|F|N|S|E|W|) (n: string) =
    if n.Contains("L") then
        L (extractDigit n "L")
    elif n.Contains("R") then
        R  (extractDigit n "R")
    elif n.Contains("F") then
        F (extractDigit n "F")
    elif n.Contains("N") then
        N (extractDigit n "N")
    elif n.Contains("S") then
        S (extractDigit n "S")
    elif n.Contains("E") then
        E (extractDigit n "E")
    elif n.Contains("W") then
        W (extractDigit n "W")
    else
        failwith "Invalid input"

let rec getHeading (dir: Turn) n (heading: Heading) =
    match dir, n with
    | _, 0 -> heading
    | Left, _ -> getHeading dir (n - 90) (turnLeft heading)
    | Right, _ -> getHeading dir (n - 90) (turnRight heading)

let moveForward heading n x y =
    match heading with
    | North -> x, y+n
    | East -> x+n, y
    | South -> x, y-n
    | West  -> x-n, y

let rec parseDirections directions heading x y =
    match directions with
    | [] -> x, y
    | head::tail  ->
        match head with
        | N n -> parseDirections tail heading x (y + n)
        | E n -> parseDirections tail heading (x + n) y
        | S n -> parseDirections tail heading x (y - n)
        | W n -> parseDirections tail heading (x - n) y
        | L n -> parseDirections tail (getHeading Left n heading) x y
        | R n -> parseDirections tail (getHeading Right n heading) x y
        | F n ->
            let nx, ny = moveForward heading n x y 
            parseDirections tail heading nx ny

let rec rotate direction degree (wx, wy) = 
    match direction, degree with 
    | _, 0 -> (wx, wy)
    | Left, n -> rotate Left (n - 90) (-wy, wx)
    | Right, n -> rotate Right (n - 90) (wy, -wx)

let rec parseDirections' directions (wx, wy) (sx, sy) =
    printfn "ship: (%d, %d) waypoint: (%d, %d)" sx sy wx wy
    match directions with
    | [] -> sx, sy
    | head::tail ->
        match head with 
        | N n -> parseDirections' tail (wx, wy + n) (sx, sy)
        | E n -> parseDirections' tail (wx + n, wy) (sx, sy)
        | S n -> parseDirections' tail (wx, wy - n) (sx, sy)
        | W n -> parseDirections' tail (wx - n, wy) (sx, sy)
        | L n -> parseDirections' tail (rotate Left n (wx, wy)) (sx, sy)
        | R n -> parseDirections' tail (rotate Right n (wx, wy)) (sx, sy)
        | F n -> parseDirections' tail (wx, wy) (sx + (wx * n), sy + (wy * n))


let content =
    __SOURCE_DIRECTORY__ + "/input.txt"
    |> File.ReadAllLines


[<EntryPoint>]
let main argv =
    parseDirections' (Array.toList content) (10, 1) (0, 0)
    |> fun (x, y) -> printfn "%d %d: %d" x y (abs x+ abs y)

    0 // return an integer exit code