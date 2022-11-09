module Tests
open Expecto

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
                | (One x)::tail -> aux (acc @ [x]) tail 
                | (Many x)::tail -> aux (aux acc x) tail 
            aux [] list
        let expected = ["a"; "b"; "c"; "d"; "e"]
        let input = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]

        Expect.sequenceEqual (flatten input) expected ""

let test08 = 
    // Eliminate consecutive duplicates of list elements.
    testCase "08 Eliminate duplicates"
    <| fun _ -> 
        let input = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
        let expected = ["a"; "b"; "c"; "a"; "d"; "e"]

        let rec compress = function 
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
        let input = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
        let expected = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];["e"; "e"; "e"; "e"]]

        let pack list = 
            let rec aux current acc = function 
            | [] -> [] 
            | [x] -> (x :: current) :: acc 
            | a :: (b :: _ as t) -> 
                if a = b then 
                    aux (a :: current) acc t 
                else 
                    aux [] ((a :: current) :: acc) t 

            (List.rev (aux [] [] list))
        Expect.sequenceEqual (pack input) expected ""


        

[<Tests>]
let tests = testList "Working with List" [ test07; test08; test09 ]
