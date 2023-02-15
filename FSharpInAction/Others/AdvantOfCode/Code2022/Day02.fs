namespace Others.AdventOfCode.Code2022
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
                    Others.AdventOfCode.Common.readInput "2022\input\day02.txt"
                    |> List.ofArray
                    |> computeInstructions
                Expect.equal scores 13924 "part one"

        [<Tests>]
        let tests = testList "Day02: part one" [test01; test02]

    module Part02 = 
        let parseCard card = 
            match card with 
            | "A" -> Rock 
            | "B" -> Paper
            | "C" -> Scissors
            | _ -> failwith $"invalid card {card}" 

        let toWin card =
            match card with 
            | Rock -> Paper 
            | Paper -> Scissors
            | Scissors -> Rock

        let toLose card =
            match card with 
            | Rock -> Scissors
            | Paper -> Rock
            | Scissors -> Paper

        let parseInstruction (opponent: string) (you: string) =
            match opponent, you with 
            | n, "Y" -> 
                // I need to draw
                parseCard n, parseCard n 
            | n, "X" ->
                // I need to lose 
                parseCard n, (toLose (parseCard n))
            | n, "Z" -> 
                // I need to win
                parseCard n, (toWin (parseCard n))
            | n, m -> 
                failwith $"invalid instruction {n}, {m}"

        let computeRound (opponent: string) (you: string) =
            let opponentCard, youCard = parseInstruction opponent you
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
            testCase "Part two: baseline"
            <| fun _ -> 
                let scores = computeInstructions ["A Y"; "B X"; "C Z"; ""]
                Expect.equal scores 12 "baseline scores"   
                
        let test02  = 
            testCase "Part two: input" 
            <| fun _ -> 
                let scores = 
                    Others.AdventOfCode.Common.readInput "2022\input\day02.txt"
                    |> List.ofArray
                    |> computeInstructions
                Expect.equal scores 13448 "Part two: input score"
        [<Tests>]
        let tests = testList "Day02: part two" [test01; test02]


