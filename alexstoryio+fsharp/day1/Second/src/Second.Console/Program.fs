open System.IO

let contents = 
    __SOURCE_DIRECTORY__ + "/input.txt"
    |> File.ReadAllLines 
    |> Array.map int

let mutable a, b, c = 0, 0, 0

for x in contents do
    for y in contents do
       for z in contents do
           if x + y + z = 2020 
                then 
                    a <- x
                    b <- y
                    c <- z
                else () 
            |> ignore
                

[<EntryPoint>]
let main argv =
    printfn "%d, %d, %d,  %d" a b c (a*b*c)
    0 // return an integer exit code