namespace AdventOfCode2022
open Expecto 

module Day05 = 

    module Part01 = 
        let test01 = 
            testCase "baseline"
            <| fun _ -> 
                Expect.isTrue true "baseline"

    [<Tests>]
    let tests = testList "Day05" [Part01.test01]

