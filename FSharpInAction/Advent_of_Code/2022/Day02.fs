namespace AdventOfCode2022
module Day02 = 

    open Expecto

    // For https://adventofcode.com/2022/day/2

    type Card = 
        | Rock
        | Paper
        | Scissors 

    let cardScore (c: Card) = 
        match c with 
        | Rock -> 1 
        | Paper -> 2 
        | Scissors -> 3

    module Part01 =
        let parseInstruction (a: string) = 
            match a with 
            | "A" -> Rock 
            | "B" -> Paper
            | "C" -> Scissors
            | "X" -> Rock
            | "Y" -> Paper
            | "Z" -> Scissors
            | _ -> 
                failwith $"invalid instruction {a}"

        let computeScore (opponent: Card) (you: Card) =
            match opponent, you with 
            | a, b when a = b -> 
                3 + (cardScore a)
            | Rock, Scissors -> 
                0 + (cardScore Scissors)
            | Paper, Rock -> 
                0 + (cardScore Rock)
            | Scissors, Paper -> 
                0 + (cardScore Paper)
            | _, card -> 
                6 + (cardScore card)

        let computeRound (opponent: string) (you: string) =
            let opponentCard = parseInstruction opponent 
            let youCard = parseInstruction you 

            computeScore opponentCard youCard

        let computeInstructions (instructions: string list) = 
            instructions
            |> List.map (fun each -> 
                each.Split " "
            )
            |> List.filter (fun each -> 
                each.Length = 2)
            |> List.map (fun each ->
                computeRound each[0] each[1]
            )
            |> List.sum

        let test01 = 
            testCase "Baseline"
            <| fun _ -> 
                Expect.equal (computeRound "A" "Y") 8 "baseline 02"
                Expect.equal (computeRound "B" "X") 1 "baseline 02"
                Expect.equal (computeRound "C" "Z") 6 "baseline 03"

                let scores = computeInstructions ["A Y"; "B X"; "C Z"; ""]
                Expect.equal scores 15 "baseline scores"


        let test02  = 
            testCase "Part one" 
            <| fun _ -> 
                let scores = 
                    AdventOfCode.Common.readInput "2022\input\day02.txt"
                    |> List.ofArray
                    |> computeInstructions
                Expect.equal scores 13924 "part one"

    [<Tests>]
    let tests = testList "Day02" [Part01.test01; Part01.test02]