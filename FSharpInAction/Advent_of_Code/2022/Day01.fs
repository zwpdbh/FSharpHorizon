module Day01

open System
open System.IO
open Expecto


// This function group non-empty "number" into a list into another list
let groupCalorie (numbers: string list) =
    let rec helper acc current =
        function
        | head :: tail when head <> "" -> helper acc (head :: current) tail
        | head :: tail when head = "" -> helper (acc @ [ current ]) [] tail
        | _ -> acc

    helper [] [] numbers

let findMaxCalorie (numbers: string array) =
    numbers
    |> List.ofArray
    |> groupCalorie
    |> List.map (fun each -> each |> List.map (fun x -> int x) |> List.sum)
    |> List.sortDescending
    |> List.head


// For https://adventofcode.com/2022/day/1
let test01 =
    testCase "Part one"
    <| fun _ -> 
        let workingDirectory = Environment.CurrentDirectory
        // We need to go to its "../../" because it is running from output folder
        let projectFolder = Directory.GetParent(workingDirectory).Parent.Parent.FullName

        let numbers =
            File.ReadAllLines(Path.Combine(projectFolder, @"2022\input\day01.txt"))
        Expect.equal (findMaxCalorie numbers) 71506 "The most total Calories carried by Elf"


[<Tests>]
let tests = testList "Day01" [ test01 ]
