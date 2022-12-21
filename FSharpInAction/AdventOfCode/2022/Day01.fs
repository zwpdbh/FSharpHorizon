module Day01

open Expecto

// For https://adventofcode.com/2022/day/1
open System
open System.IO


let workingDirectory = Environment.CurrentDirectory
let projectFolder = Directory.GetParent(workingDirectory).FullName

//OR 
//let numbers = File.ReadAllLines(@".\input\day01.txt") 
let numbers =
    File.ReadAllLines(Path.Combine(projectFolder, @"2022\input\day01.txt"))


// This function group non-empty "number" into a list into another list
let groupCalorie (numbers: string list) = 
    let rec helper acc current = function 
        | head::tail when head <> ""-> 
            helper acc (head::current) tail    
        | head::tail when head = ""-> 
            helper (acc @  [current]) [] tail 
        | _ -> 
            acc 
    helper [] [] numbers

let findMaxCalorie (numbers: string array) = 
    numbers
    |> List.ofArray
    |> groupCalorie
    |> List.map (fun each -> 
        each
        |> List.map (fun x -> int x)
        |> List.sum 
        )
    |> List.sortDescending
    |> List.head


let test01 = 
    testCase "Part one" 
    <| fun _ -> 
        Expect.isTrue true ""
        Expect.equal (findMaxCalorie numbers) 71506 "The most total Calories carried by Elf"

[<Tests>]
let tests = testList "Day01" [ test01 ]