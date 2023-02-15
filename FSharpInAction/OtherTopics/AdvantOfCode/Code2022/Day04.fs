namespace Others.AdventOfCode.Code2022
open Expecto

module Day04 = 
    let splitLineWith (c: char) (line: string) =
        Seq.toList (line.Split c)

    let convertToSet (lst: int list) = 
        match lst with 
        | x::y::_ -> 
            [x..y]
        | _ -> 
            failwith "list should just contain at least two elements"

    // assignment is something like: "15-60"
    let sectionAssigmentToSet (assignment: string) = 
        assignment
        |> splitLineWith '-'
        |> List.map (fun each -> int each)
        |> convertToSet
        |> Set.ofList



    let input = 
        (Others.AdventOfCode.Common.readInput "2022/input/day04.txt")
        |> List.ofArray
        |> List.filter (fun each -> each.Length > 0)

    let testInput = 
        testCase "test input"
        <| fun _ -> 
            Expect.equal input.Length 1000 "input length"


    module Part01 = 
        let firstTwoSetFullyOverlap lst =
            match lst with 
            | set01::set02::_ ->
                let intersect = Set.intersect set01 set02
                match intersect with 
                | _ when intersect = set01 -> true 
                | _ when intersect = set02 -> true 
                | _ -> false 
            | _ -> 
                failwith "lst should contain at least two set"

        let onePairFullyOverlap (s:string) = 
            s
            |> splitLineWith ','
            |> List.map (fun each -> 
                sectionAssigmentToSet each 
            )
            |> firstTwoSetFullyOverlap

        let test01 = 
            testCase "Part01 baseline"
            <| fun _-> 
                Expect.isTrue (onePairFullyOverlap "6-6,2-6") ""
                Expect.isTrue (onePairFullyOverlap "2-8,3-7") ""
                Expect.isFalse (onePairFullyOverlap "2-6,4-8") ""

        let test02 = 
            testCase "Part01 with input"
            <| fun _ ->
                let result = 
                    input 
                    |> List.map (fun each ->
                        match onePairFullyOverlap each with 
                        | true -> 1 
                        | false -> 0
                    )
                    |> List.sum
                
                Expect.equal result 466 "assignment pairs does one range fully contain the other"

    module Part02 = 
        let firstTwoSetFullyOverlap lst =
            match lst with 
            | set01::set02::_ ->
                let intersect = Set.intersect set01 set02
                match intersect with 
                | _ when intersect <> Set.empty -> true 
                | _ -> false
            | _ -> 
                failwith "lst should contain at least two set"

        let onePairFullyOverlap (s:string) = 
            s
            |> splitLineWith ','
            |> List.map (fun each -> 
                sectionAssigmentToSet each 
            )
            |> firstTwoSetFullyOverlap

        let test01 = 
            testCase "Part02 with input"
            <| fun _ ->
                let result = 
                    input 
                    |> List.map (fun each ->
                        match onePairFullyOverlap each with 
                        | true -> 1 
                        | false -> 0
                    )
                    |> List.sum
                
                Expect.equal result 865 "assignment pairs do the ranges overlap"

    [<Tests>]
    let tests = testList "Day04" [testInput; Part01.test01; Part01.test02; Part02.test01]

    // Lesson 01
    // To fit "|>" especially during processing of list, we shall keep the function in each stage accept list
    // as parameter and handle details in side to avoid breaking the "shape" of process flow.
    // Just as we did for "firstTwoSetFullyOverlap"

    // Lesson 02 
    // List to Set and Set operations
