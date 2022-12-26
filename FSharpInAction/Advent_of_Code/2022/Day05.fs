namespace AdventOfCode2022
open Expecto 

// For https://adventofcode.com/2022/day/5
module Day05 = 
    type Command = {Move: int; From: int; To: int}

    let parseCommand (str: string) = 
        let command = 
            str.Split ' '
            |> Array.choose (fun each -> 
                try 
                    each |> int |> Some
                with 
                    _ -> None 
            )
        {
            Move = command[0]
            From = command[1]
            To = command[2]
        }

    let messageFromCargo (cargo:  char list array)= 
        cargo 
        |> Array.choose (fun eachList -> 
            List.tryHead eachList
        )
        |> Array.ofSeq
        
    let moveCrateFromToTarget (n:int) (from: 'a list) (target: 'a list) = 
        let moved = 
            from[..(n-1)]
            |> List.rev 

        let left = from[n..]
        let target' = moved @ target

        left, target'
            

    let moveCargoFrom (c: Command) (cargo: char list array) = 
        let from = cargo[c.From - 1]
        let target = cargo[c.To - 1]

        let (updatedFrom, updatedTarget) = moveCrateFromToTarget c.Move from target
        cargo[c.From - 1] <- updatedFrom
        cargo[c.To - 1] <- updatedTarget
        cargo

    let moveCargoFromInput cargo input  = 
        let mutable cargo = cargo
        input 
        |> List.map (fun each -> parseCommand each)
        |> List.iter (fun eachCommand -> 
            cargo <- moveCargoFrom eachCommand cargo
        )
        cargo 

    let cargoInput = 
                                """
                            [Q]     [G]     [M]    
                            [B] [S] [V]     [P] [R]
                    [T]     [C] [F] [L]     [V] [N]
                [Q] [P]     [H] [N] [S]     [W] [C]
                [F] [G] [B] [J] [B] [N]     [Z] [L]
                [L] [Q] [Q] [Z] [M] [Q] [F] [G] [D]
                [S] [Z] [M] [G] [H] [C] [C] [H] [Z]
                [R] [N] [S] [T] [P] [P] [W] [Q] [G]
                 1   2   3   4   5   6   7   8   9 
                                """

    module Part01 = 
        let test01 = 
            testCase "baseline"
            <| fun _ -> 
                let cargo = [|['N'; 'Z'];['D';'C';'M'];['P']|]
                let commandsInput = [
                    "move 1 from 2 to 1"
                    "move 3 from 1 to 3"
                    "move 2 from 2 to 1"
                    "move 1 from 1 to 2"
                ]
                let message = 
                    commandsInput
                    |> moveCargoFromInput cargo
                    |> messageFromCargo
                    |> System.String

                Expect.equal message "CMZ" "baseline from part one"



        let test02 =
            testCase "part one input"
            <| fun _ -> 
                Expect.isTrue true ""

    [<Tests>]
    let tests = testList "Day05" [Part01.test01]


    // Things to learn
    // String to chars , then from chars to string: List.ofSeq "abcd" |> List.toArray |> System.String |> printfn "%A"

