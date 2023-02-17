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

        // Determin if a given string which contains ONLY parentheses charaters
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


    module Turing02 = 
        module Problem01 = 
            open System.Text.RegularExpressions
            let freq s = 
                Regex.Matches(s, @"\S+")
                |> Seq.cast<Match>
                |> Seq.map (fun m -> m.ToString ())
                |> Seq.groupBy id 
                |> Seq.map (fun (k, v) -> k, Seq.length v)
                |> Map.ofSeq

            let addMap map1 map2 =
                let increment mapAcc word count =
                    match mapAcc |> Map.tryFind word with 
                    | Some count' -> mapAcc |> Map.add word (count + count')
                    | None -> mapAcc |> Map.add word count 

                map2 |> Map.fold increment map1

            let OddWords s1 s2 = 
                s1
                |> freq 
                |> addMap (s2 |> freq)
                |> Map.toList
                |> List.filter (fun (_, v) -> v = 1)
                |> List.map (fun (k, _) -> k)

            let test01 = 
                "turing community is turing is the best" |> freq

            let test02 = 
                OddWords "turing community is turing is the best" "turing community is turing is greatest"

        module Problem02 = 
            let digitsToMap s =
                s
                |> Seq.toList
                |> Seq.indexed
                |> Seq.map (fun (k, v) -> v, k)
                |> Map.ofSeq

            let numInputPair s =
                s 
                |> Seq.toList
                |> Seq.pairwise

            /// Given digitStr which only contains digits number: 8459762103
            /// Given num which represent numbers the user need to click by moving horizontally. 
            /// Compute the cost of it. 
            /// Don't forget the initial move.
            let timeNeeded (digitStr:string) (num: string) = 
                let theFirstMoveCost = digitStr.IndexOf(Seq.head num)
                let digitMap = digitsToMap digitStr
                numInputPair num 
                |> Seq.map (fun (a, b) -> 
                    match digitMap.TryFind a, digitMap.TryFind b with 
                    | Some j, Some q -> abs (j - q)
                    | _ -> 0
                )
                |> Seq.sum
                |> (+) theFirstMoveCost
             
            let test01 = 
                digitsToMap "8459762103"

            let test02 = 
                numInputPair "210"

            let test03 = 
                timeNeeded "0123456789" "210" // 4

            let test04 = 
                timeNeeded "8459762103" "5439" // 17

    [<Tests>]
    let tests = testList "Others" [Turing01.test01; Turing01.test02]

