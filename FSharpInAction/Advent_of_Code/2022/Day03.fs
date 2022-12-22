namespace AdventOfCode2022
open Expecto



module Day03 = 
    let rucksack01 = "vJrwpWtwJgWrhcsFMMfFFhFp"
    let rucksack02 = "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    let rucksack03 = "PmmdzqPrVvPwwTWBwg"
    let rucksack04 = "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
    let rucksack05 = "ttgJtRGJQctTZtZT"
    let rucksack06 = "CrZsJsPPZsGzwwsLwLmpwMDw"

    let splitComponents (s: string) = 
        s[0..(s.Length / 2 - 1)], s[(s.Length / 2)..]

    let intersectBetween (str01, str02) = 
        // From string to list of chars to set
        let set01 = str01 |> List.ofSeq |> Set.ofList  
        let set02 = str02 |> List.ofSeq |> Set.ofList 
        // From set to list
        Set.intersect set01 set02
        |> List.ofSeq 
        |> List.head

    // How to represent the map between a-z to 1-26
    let index = 
        ['a'..'z'] @ ['A'..'Z']
        |> List.indexed
        |> List.map (fun (i, c) -> (c, i + 1))
        |> Map.ofList

    let characterPriorities (c: char) = 
        index 
        |> Map.find c

    

    let computeSumOfPriorities (input: string list) = 
        let computePriority str = 
            str 
            |> splitComponents
            |> intersectBetween
            |> characterPriorities
        input 
        |> List.map (fun each -> computePriority each)
        |> List.sum

    let test01 = 
        testCase "baseline"
        <| fun _ -> 
            let component01, component02 = splitComponents rucksack01
            let component03, component04 = splitComponents rucksack02
            Expect.equal component01 "vJrwpWtwJgWr" "component01"
            Expect.equal component02 "hcsFMMfFFhFp" "component02"
            Expect.equal component03 "jqHRNqRjqzjGDLGL" "component03"
            Expect.equal component04 "rsFMfFZSrLrFZsSL" "component04"

            Expect.equal (intersectBetween (splitComponents rucksack01)) 'p' "rucksack01"
            Expect.equal (intersectBetween (splitComponents rucksack02)) 'L' "rucksack02"

            let baseline = 
                computeSumOfPriorities [rucksack01; rucksack02; rucksack03; rucksack04; rucksack05; rucksack06]
            Expect.equal baseline 157 "baseline"

    let test02 = 
        testCase "part one"
        <| fun _ -> 
            let input = 
                AdventOfCode.Common.readInput "2022/input/day03.txt"
                |> List.ofArray
                |> List.filter (fun x -> x.Length > 0)
            let score = 
                input
                |> computeSumOfPriorities

            Expect.equal input.Length 300 "lines of input"
            Expect.equal score 7763 "Part one score"

    [<Tests>]
    let tests = testList "Day03" [test01; test02]