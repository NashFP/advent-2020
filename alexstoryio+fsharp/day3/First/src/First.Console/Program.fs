namespace First

module Console =
    open System.IO

    type Spot = Tree | Plain
    let getSpot = function
    | '#' -> Tree
    | '.' -> Plain
    | _ -> failwith "Invalid Argument"

    let content = File.ReadAllLines (__SOURCE_DIRECTORY__ + "/input.txt")
    let x, y = 0, 0
    let height = Array.length content
    let trackWidth = content |> Array.head |> String.length
    let rebalanceX x = x % trackWidth


    let rec count x y xStep yStep trees =
        let xTarget = rebalanceX (x + xStep)
        let yTarget = y + yStep

        if yTarget > height - 1 then 
            trees
        else
            match getSpot content.[yTarget].[xTarget] with
            | Tree -> count xTarget yTarget xStep yStep (trees + 1)
            | Plain -> count xTarget yTarget xStep yStep trees

    let startCount xStep yStep = count 0 0 xStep yStep 0

    [<EntryPoint>]
    let main argv =
        let oneOne =  startCount 1 1 
        let threeOne = startCount 3 1 
        let fiveOne = startCount 5 1 
        let sevenOne = startCount 7 1
        let oneTwo = startCount 1 2
        
        printfn "1-1 %d" oneOne
        printfn "3-1 %d" threeOne
        printfn "5-1 %d" fiveOne
        printfn "7-1 %d" sevenOne
        printfn "1-2 %d" oneTwo
        printfn "total %A" (bigint oneOne * bigint threeOne * bigint fiveOne * bigint sevenOne * bigint oneTwo)
        0 // return an integer exit code