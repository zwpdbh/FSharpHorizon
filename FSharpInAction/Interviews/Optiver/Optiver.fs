module Optiver

open Expecto
open System
open System.IO

let workingDirectory = Environment.CurrentDirectory;
let projectFolder = Directory.GetParent(workingDirectory).Parent.Parent.FullName

let speedRecordsFile = "Optiver\speed.txt"

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

                if d >= 1000 then 
                    let mutable currD = 0
                    acc <-
                        acc 
                        |> List.rev 
                        |> List.takeWhile (fun x -> 
                            currD <- x + currD
                            currD <= 1000
                        )
                    yield acc 
        }
    segment 
    |> Seq.sortBy (fun each -> each.Length)
    |> Seq.head

let test01 = 
    testCase "01: max average speed" 
    <| fun _ -> 
        let recordFilePath = Path.Combine(projectFolder, speedRecordsFile)

        let count = loadSpeedRecord recordFilePath |> List.length
        Expect.equal count 491 "The length of speed records"

        let dis = maxAverageSpeed (loadSpeedRecord recordFilePath) |> List.sum 
        Expect.isTrue (dis >= 1000) ""
        

[<Tests>]
let tests = 
    testList "From Optiver " [test01]