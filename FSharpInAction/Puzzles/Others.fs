namespace Puzzles
open Expecto

module Others= 

    module Todos = 
        let test01 =
            testCase "01 Anagram Substring Search problem"
            <| fun _ -> Expect.isTrue true "https://theburningmonk.com/2016/12/anagram-substring-search-problem-in-fsharp/"

        let test02 = 
            testCase "02 Ransom Note problem"
            <| fun _ -> Expect.isTrue false "https://theburningmonk.com/2016/12/ransom-note-problem-in-fsharp/"


    module Turing01 = 
        // Given a list of inputs (a list of string with sigal charaters)
        // + means record a new score that is the sum of the previous two scores
        // D means record a new score that is double the previous score.
        // C invliadate the previous score, remove it from score records
        // Compute the total score given something like: ["5"; "2"; "C"; "D"; "+"]
        let scores inputs = 
            let rec helper inputs curr  = 
                printfn $"inputs = {inputs}, curr = {curr}"
                match inputs, curr with 
                | x::restInputs, m::n::_ when x = "+" -> 
                    let sum = (int m) + int (n)
                    helper restInputs (sum :: curr )
                | x::restInputs, y::_ when x = "D" -> 
                    let yInt = int y 
                    helper restInputs (yInt * 2 :: curr) 
                | x::restInputs, y::restCurr when x = "C" -> 
                    helper restInputs restCurr
                | x::restInputs, _ -> 
                    helper restInputs ((x |> int)::curr) 
                | _ ->
                    curr

            helper inputs []
            |> List.sum

        let test01 =
            testCase "Baseball game"
            <| fun _ -> 
                Expect.equal (scores ["5"; "2"; "C"; "D"; "+"]) 30 ""
                Expect.equal (scores ["5"; "-2"; "4"; "C"; "D"; "9"; "+"; "+"]) 27 ""

        // Determin if a given string which contains only parentheses charaters
        // is valid or not
        let isValidParentheses (str: string) = 
            let inputs = str |> Seq.toList

            let rec helper inputs curr = 
                printfn $"inputs = {inputs}, curr = {curr}"
                match inputs, curr with 
                | x::restInputs, y::restCurr when x = ')' && y = '(' -> 
                    helper restInputs restCurr 
                | x::restInputs, y::restCurr when x = '}' && y = '{' -> 
                    helper restInputs restCurr 
                | x::restInputs, y::restCurr when x = ']' && y = '[' -> 
                    helper restInputs restCurr 
                | x::restInputs, _ when (x = '(' || x = '{' || x = '[') -> 
                    helper restInputs (x :: curr)
                | _ -> 
                    curr 

            (helper inputs []) 
            |> List.isEmpty

        let test02 = 
            testCase "Valid Paratheses string"
            <| fun _ -> 
                Expect.isTrue (isValidParentheses "()") ""
                Expect.isTrue (isValidParentheses "()[]{}") ""
                Expect.isFalse (isValidParentheses "(]") ""
                Expect.isFalse (isValidParentheses "([)]") ""
                Expect.isTrue (isValidParentheses "{[]}") ""


    [<Tests>]
    let tests = testList "Others" [Turing01.test01; Turing01.test02]

