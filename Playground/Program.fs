
open DemoREPerformance
open FunAndProfit

//let n = 5000 * 5

//timeRegulareExpression n |> printfn "regulare expression takes: %A" 
//timeStringContain n |> printfn "string contains takes: %A" 


open System.IO


let speedFile = @"D:\code\fsharp-programming\FSharpHorizen\FSharpInAction\Interviews\Optiver\speed.txt"

let loadSpeedRecord filePath = 
    File.ReadAllLines filePath 
    |> List.ofArray 
    |> List.filter (fun x -> x.Trim() <> "")
    |> List.map (fun x -> int x)

// Find the maximum average speed over 1 km
// Each element in the list is a distance run in 5 mins
let maxAverageSpeed (lst: int list) = 
    let mutable d = 0
    let mutable acc = [] : int list 
    let segment = 
        seq {        
            for v in lst do 
                d <- d + v 
                acc <- acc @ [v]

                if d > 1000 then 
                    let mutable currD = 0
                     
                    acc <-
                        acc 
                        |> List.rev 
                        |> List.takeWhile (fun x -> 
                            currD <- x + currD
                            currD <= 1000
                        )
                        |> List.rev 
                    yield acc
                    
        }
    segment 
    |> Seq.sortBy (fun each -> each.Length)


let result = 
    maxAverageSpeed (loadSpeedRecord speedFile)

for each in result do 
    printfn $"{each} : {List.sum each}"