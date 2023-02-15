namespace Puzzles

open Expecto

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

        let test01 =
            testCase "Eight Queens problem"
            <| fun _ -> Expect.isTrue true ""

    [<Tests>]
    let tests = testList "BackTracking" [ EightQueens.test01 ]
