module Optiver

open Expecto

let speedRecordsPath = "@.\Optiver\speed.txt"

//let loadSpeedRecord filePath = 
//    System.IO.

let test01 = 
    testCase "01: max average speed" 
    <| fun _ -> 
        Expect.isTrue true ""

[<Tests>]
let tests = 
    testList "From Optiver " [test01]