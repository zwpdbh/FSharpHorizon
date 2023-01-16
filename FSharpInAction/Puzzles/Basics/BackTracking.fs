namespace Puzzles
open Expecto

module BackTracking = 
    module EightQueens = 

        type explored = bool

        let noConflict (k: int) (curr: int list) = 
            // Each one in curr represent a placed queen's postion in a row. All curr elements are valid for now.
            // The function is to decide whether the newest one:  is conflict with other rows
            let safeOne = 
                curr 
                |> List.indexed
                |> List.filter (fun (i, v) -> 
                    // To check if any one is vertical or diagonal to newest one: (curr.Length, k)
                    match v = k, abs(v - k) = abs(curr.Length - i) with 
                    | false, false -> true 
                    | _, _ -> false 
                )
                |> List.tryHead

            match safeOne with 
            | Some _ -> true 
            | None -> false 

        // Find all solution for N Queens 
        let nQueen n = 
            // We need to record down where has been explored and whether it is safe to place a queen
            let lookup: Map<int * int, bool>  = Map.empty
            let allSolution: int list list = []

            let rec helper (lookup: Map<int * int, bool>) (curr: int list) = 
                if curr.Length = n then
                    allSolution <- curr :: allSolution
                    Some curr 
                else 
                    // try all choices, 
                    for i in [1..n] do 
                        if noConflict i curr then 
                            lookup <- lookup.Add((r, curr.Length), true)
                            match helper lookup (i::curr) with 
                            | Some sol -> 
                                Some sol 
                            | None -> 
                                lookup <- lookup.Remove(r, curr.Length)
                                None 
                        else 
                            None 

        // See: https://codereview.stackexchange.com/questions/53752/making-backtracking-sudoku-solver-more-functional
        // About backtracking 

                            
                        





        let test01 = 
            testCase "Eight Queens problem"
            <| fun _ ->
                Expect.isTrue true ""

    [<Tests>]
    let tests = testList "BackTracking" [EightQueens.test01]  

