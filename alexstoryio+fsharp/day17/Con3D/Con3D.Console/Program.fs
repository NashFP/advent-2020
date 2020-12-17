open System.IO
open FSharpPlus

type Point =
    {
        X: int
        Y: int
        Z: int
        W: int
    }

type Cube = Point[]

module Cube =
    let getNeighbors point =
        [|
            for x in [point.X-1 .. point.X+1] do
                for y in [point.Y-1 .. point.Y+1]do
                    for z in [point.Z-1 .. point.Z+1] do
                        for w in [point.W-1 .. point.W+1] do
                            if x <> point.X || y <> point.Y || z <> point.Z || point.W <> w then
                                yield sprintf "%d:%d:%d:%d" x y z w
        |]

    let getActiveSpots (cube: Cube) =
        cube
        |> fold (fun acc p -> Array.append acc (getNeighbors p)) [||]
        |> Array.countBy id

    let fromString text =
        let parted = String.split [|":"|] text |> Array.ofSeq
        {
            X = int parted.[0]
            Y = int parted.[1]
            Z = int parted.[2]
            W = int parted.[3]
        }

    let isActive cube pnt =
        match snd pnt with
        | 3 -> true
        | 2 -> Array.contains (fromString (fst pnt)) cube
        | _ -> false

    let step cube =
        cube
        |> getActiveSpots
        |> filter (isActive cube)
        |> map (fst >> fromString)


let content: Cube = 
    let active = 
        __SOURCE_DIRECTORY__ + "/input.txt"
        |> File.ReadAllLines
        |> map ( String.toArray)
    
    [|
        for yi, y in Array.indexed active do
            for xi, x in Array.indexed y do
                if x = '#' then
                    yield 
                        {
                            X = xi
                            Y = yi
                            Z = 0
                            W = 0
                        }
    |]

[<EntryPoint>]
let main argv =
    content
    |> Cube.step
    |> Cube.step
    |> Cube.step
    |> Cube.step
    |> Cube.step
    |> Cube.step
    |> length
    |> printfn "%A"
    0 