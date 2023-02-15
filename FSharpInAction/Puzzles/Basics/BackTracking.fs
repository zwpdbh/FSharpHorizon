namespace Puzzles

open Expecto

/// From https://medium.com/analytics-vidhya/the-blueprint-to-solve-any-backtracking-problem-b3640a3dcbd7
module BackTracking =
    module EightQueens =

        // See: https://codereview.stackexchange.com/questions/53752/making-backtracking-sudoku-solver-more-functional
        // About backtracking

        /// From http://www.fssnip.net/7PT/title/Solving-8-queens-problem-with-F
        /// Generate all X,Y coordinates on the board
        /// (initially, all of them are available)
        let n = 4
        let k = n - 1
        let all = 
            [ for x in 0..k do 
                for y in 0..k do 
                    yield x, y
            ]

        /// Given available positions on the board, filter
        /// out those that are taken by a newly added queen
        /// at the position qx, qy
        let filterAvailable (qx, qy) available =
            available
            |> List.filter (fun (x, y) ->
                // horizontal & vertical
                x <> qx
                && y <> qy
                &&
                // two diagonals
                (x - y) <> (qx - qy)
                && (k - x - y) <> (k - qx - qy))

        /// Generate all solutions. Given already added queens
        /// and remaining available positions, we handle 3 cases:
        ///  1. we have 8 queens - yield their locations
        ///  2. we have no available places - nothing :(
        ///  3. we have available place
        let rec solve queens available =
            seq {
                match queens, available with
                | q, _ when List.length q = n -> yield queens
                | _, [] -> ()
                | _, a :: available ->
                    // generate all solutions with queen at 'a'
                    yield! solve (a :: queens) (filterAvailable a available)
                    // generate all solutions with nothing at 'a'
                    yield! solve queens available
            }

        /// Nicely render the queen locations
        let render items =
            let arr = Array.init n (fun _ -> Array.create n " . ")

            for x, y in items do
                arr[x][y] <- " x "

            for a in arr do
                a |> String.concat "" |> printfn "%s"

            String.replicate (n * 3) "-" |> printfn "%s"

        // Print all solutions :-)
        let demoAllNQueenSolutions () = 
            solve [] all |> Seq.iter render 

        let demoOneNQueenSolutions () = 
            solve [] all |> Seq.head |> render


    module Permuations =
        /// Given an seq nums of distinct integers, return all the possible permutations. You can return the answer in any order.

        let rec insert x l = 
            seq {
                match l with 
                | [] -> yield [x]
                | y::rest ->
                    yield x :: l 
                    for i in insert x rest do 
                        yield y :: i 
            }

        let rec permutation l = 
            seq {
                match l with 
                | [] -> []
                | x :: tail -> 
                    for each in permutation tail do 
                        yield! insert x each 
            }

        let demoPermutation () = 
            permutation [1;2;3;4]
            |> Seq.length


        let test = 
            let rec insert x l = 
                [
                    match l with 
                    | [] -> yield [x]
                    | y::rest ->
                        yield x :: l 
                        for i in insert x rest do 
                            yield y :: i 
                ]

            let rec permutation l = 
                [
                    match l with 
                    | [] -> []
                    | x :: tail -> 
                        for each in permutation tail do 
                            yield! insert x each 
                ]

            permutation [1;2;3;4]
            |> Seq.length

    module Subsets = 
        // Given an integer list which is  nums of unique elements, return all possible subsets (the power set).

        let rec subsets s = 
            set [
                yield s 
                for e in s do 
                    yield! subsets (Set.remove e s)
            ]

        let demoSubsets () = 
            subsets (set [1; 2; 3])
            |> printfn "%A"

    /// More about sequence: 
    /// http://www.fssnip.net/7XL/title/Continue-sequence-after-applying-SeqtakeWhile-skip-take-etc

    [<Tests>]
    let tests = testList "BackTracking" [ ]
