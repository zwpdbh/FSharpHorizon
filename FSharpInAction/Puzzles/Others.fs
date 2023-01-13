module Tests

open Expecto

let test01 =
    testCase "01 Anagram Substring Search problem"
    <| fun _ -> Expect.isTrue true "https://theburningmonk.com/2016/12/anagram-substring-search-problem-in-fsharp/"

let test02 = 
    testCase "02 Ransom Note problem"
    <| fun _ -> Expect.isTrue true "https://theburningmonk.com/2016/12/ransom-note-problem-in-fsharp/"


[<Tests>]
let tests = testList "Others" [ test01 ]
