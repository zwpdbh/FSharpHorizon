module Day01

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

let findTop3TotalCalorie (numbers: string array) = 
    numbers
    |> List.ofArray
    |> groupCalorie
    |> List.map (fun each -> each |> List.map (fun x -> int x) |> List.sum)
    |> List.sortDescending
    |> List.indexed
    |> List.takeWhile (fun (i, _) -> i < 3)
    |> List.sumBy (fun (_, v) -> v)



let numbers =
    AdventOfCode.Common.readInput "2022\input\day01.txt"

// For https://adventofcode.com/2022/day/1
let test01 =
    testCase "Part one"
    <| fun _ -> 
        Expect.equal (findMaxCalorie numbers) 71506 "The most total Calories carried by Elf"


let test02 =
    testCase "Part two"
    <| fun _ -> 
        Expect.equal (findTop3TotalCalorie numbers) 209603 "The most total Calories carried by Elf"

[<Tests>]
let tests = testList "Day01" [ test01; test02 ]
