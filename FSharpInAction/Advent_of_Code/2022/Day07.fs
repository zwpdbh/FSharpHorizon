namespace AdventOfCode2022

module Day07 = 
    open Expecto

    module Part01 = 
        let test01 = 
            testCase "part01 baseline"
            <| fun _ -> 
                Expect.isTrue true ""

    [<Tests>]
    let tests = testList "Day 07" [Part01.test01]

