namespace Others.Problems99 

module Tests =

    open Expecto

    /// FROM: https://ocaml.org/problems

    type 'a node =
        | One of 'a
        | Many of 'a node list

    let test07 =
        testCase "07 Flatten a list"
        <| fun _ ->
            // May have problem running in REPL https://github.com/dotnet/fsharp/issues/14216
            let flatten (list: string node list) =
                let rec aux acc lst =
                    match lst with
                    | [] -> acc
                    | (One x) :: tail -> aux (acc @ [ x ]) tail
                    | (Many x) :: tail -> aux (aux acc x) tail

                aux [] list

            let expected = [ "a"; "b"; "c"; "d"; "e" ]

            let input =
                [ One "a"
                  Many [ One "b"
                         Many [ One "c"; One "d" ]
                         One "e" ] ]

            Expect.sequenceEqual (flatten input) expected ""

    let test08 =
        // Eliminate consecutive duplicates of list elements.
        testCase "08 Eliminate duplicates"
        <| fun _ ->
            let input =
                [ "a"
                  "a"
                  "a"
                  "a"
                  "b"
                  "c"
                  "c"
                  "a"
                  "a"
                  "d"
                  "e"
                  "e"
                  "e"
                  "e" ]

            let expected = [ "a"; "b"; "c"; "a"; "d"; "e" ]

            let rec compress =
                function
                | a :: (b :: _ as tail) ->
                    // Instead of keep the first and ignore following repeated as I first though
                    // It continue to see next until meet different element to keep the last one !
                    if a = b then
                        compress tail
                    else
                        a :: (compress tail)
                | x -> x


            Expect.sequenceEqual (compress input) expected ""


    let test09 =
        // Pack consecutive duplicates of list elements into sublists.
        testCase "09 Pack consecutive duplicates"
        <| fun _ ->
            let input =
                [ "a"
                  "a"
                  "a"
                  "a"
                  "b"
                  "c"
                  "c"
                  "a"
                  "a"
                  "d"
                  "d"
                  "e"
                  "e"
                  "e"
                  "e" ]

            let expected =
                [ [ "a"; "a"; "a"; "a" ]
                  [ "b" ]
                  [ "c"; "c" ]
                  [ "a"; "a" ]
                  [ "d"; "d" ]
                  [ "e"; "e"; "e"; "e" ] ]

            let pack list =
                // Here, current is used to collect current dupicated processing elements
                let rec aux current acc =
                    function
                    | [] -> []
                    | [ x ] -> (x :: current) :: acc // Because there is just last one element, we consider it is duplicated, therefor we move it into current and then move it into acc.
                    | a :: (b :: _ as t) ->
                        if a = b then
                            aux (a :: current) acc t // 1) when there are duplicated element, we need to move it into current
                        else
                            aux [] ((a :: current) :: acc) t //2 when there is no duplicated element (from some element a to element b), we need to clear current and move processing one into acc with previous current.

                (List.rev (aux [] [] list))

            Expect.sequenceEqual (pack input) expected ""


    let test10 =
        testCase "Run-length encoding"
        <| fun _ ->
            let expected =
                [ (4, "a")
                  (1, "b")
                  (2, "c")
                  (2, "a")
                  (1, "d")
                  (4, "e") ]

            let input =
                [ "a"
                  "a"
                  "a"
                  "a"
                  "b"
                  "c"
                  "c"
                  "a"
                  "a"
                  "d"
                  "e"
                  "e"
                  "e"
                  "e" ]

            let encode list =
                let rec aux current acc =
                    function
                    | _ -> [ (4, "1") ]

                aux [] [] list

            Expect.isTrue true ""



    [<Tests>]
    let tests = testList "Working with List" [ test07; test08; test09 ]
