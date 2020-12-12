open System.IO
open System.Text.RegularExpressions

type Color = string
type BagCapacity = Color * int
type Bag = {
    Color: Color
    Holds: BagCapacity list
}

let emptyRegex = "no other bags"
let colorRegex = "^.+?(?=\sbags\s)"
let heldBagsRegex = "(?<=contain\s|,\s)(\d+)\s(.+?)(?=\sbag)"

let createBag bagText = 
    let colorMatch = Regex.Match(bagText, colorRegex)
    if Regex.IsMatch(bagText, emptyRegex) then
        { Color = colorMatch.Groups.[0].ToString(); Holds = []}  
    else 
        let bagMatch = Regex.Matches(bagText, heldBagsRegex)
        let bags = [for bag in bagMatch -> (bag.Groups.[2].ToString(), int (bag.Groups.[1].ToString()))]
        {Color = colorMatch.Groups.[0].ToString(); Holds = bags}   


let content =
    __SOURCE_DIRECTORY__ + "/input.txt"
    |> File.ReadAllLines
    |> Seq.map createBag

let getBag color =
    Seq.find (fun x -> x.Color = color) content

let rec getBagCount color bag n =
    match bag.Holds with
    | [] -> n
    | bags -> Seq.sumBy (fun x -> getBagCount' x color) bags

and getBagCount'  cap color =
    let c, num = cap
    if c = color then
        num 
    else
        (getBagCount color (getBag c) 0) * num
    

let bagCount color bag =
    getBagCount color bag 0


let rec getInnerBags bag =
    match bag.Holds with
    | [] -> 0
    | bags -> Seq.sumBy (fun (c, n) -> n + (getInnerBags (getBag c)) * n) bags

let innerBags bag = getInnerBags bag

// let path1 = 
//     content
//     |> Seq.map (bagCount "shiny gold")
//     |> Seq.filter (fun x -> x > 0)
//     |> Seq.length
    
[<EntryPoint>]
let main argv =
    "shiny gold"
    |> getBag
    |> innerBags
    |> printfn "%A"
    0 // return an integer exit code