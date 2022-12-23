namespace AdventOfCode2022
open Expecto

module Day04 = 
    module Part01 = 
        let test01 = 
            testCase "Part01 baseline"
            <| fun _-> 
                Expect.isTrue true ""


    [<Tests>]
    let tests = testList "Day04" [Part01.test01]

