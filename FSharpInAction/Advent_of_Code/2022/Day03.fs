namespace AdventOfCode2022
open Expecto

module Day03 = 
    let test01 = 
        testCase "baseline"
        <| fun _ -> 
            Expect.isTrue true ""


    [<Tests>]
    let tests = testList "Day03" [test01]