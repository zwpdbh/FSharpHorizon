module Tests

open Expecto

type 'a node = 
    | One of 'a 
    | Many of 'a node list 



let test07 =
    testCase "problem 07 flatten a list"
    <| fun _ -> 
        let expected = ["a"; "b"; "c"; "d"; "e"]
        let input = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]

        let flatten (lst: 'a node list) = 
            ["a"]
        Expect.sequenceEqual (flatten input) expected ""

[<Tests>]
let tests = testList "Working with List" [ test07 ]
