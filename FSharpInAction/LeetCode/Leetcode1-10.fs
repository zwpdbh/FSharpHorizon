module Tests

open Expecto

type LinkList<'T> =
    | Empty
    | Node of value: 'T * next: LinkList<'T>

let test01 =
    testCase "01: Two sum"
    <| fun _ ->
        let twoSum (nums: int list) (target: int) =
            let numsWithIndex = List.indexed nums

            seq {
                for (i, x) in numsWithIndex do
                    for (j, y) in numsWithIndex do
                        if i < j && (x + y = target) then
                            yield (i, j)
            }

        Expect.sequenceEqual (twoSum ([ 2; 7; 11; 15 ]) 9) (seq [ (0, 1) ]) ""
        Expect.sequenceEqual (twoSum ([ 3; 2; 4 ]) 6) (seq [ (1, 2) ]) ""
        Expect.sequenceEqual (twoSum ([3; 3]) 6) (seq [(0, 1)]) ""
        Expect.isTrue true "Easy"

let test02 = 
    testCase "02: Add two number"
    <| fun _ -> 
            let convertListToNumber (lst: int list) : int = 
                let mutable n = 0.0 
                for (i, x) in lst |> List.indexed do 
                    n <- n + (10.0 ** i) * (float x)
                System.Convert.ToInt32(n)

            Expect.equal (convertListToNumber ([2; 4; 3])) 342 ""
            Expect.equal (convertListToNumber ([5; 6; 4])) 465 ""
            Expect.equal (convertListToNumber ([0; 6; 4])) 460 ""

            let convertNumToSeq (n: int) : int list = 
                let str = sprintf $"{n}"  
                seq {
                    for x in str do 
                        yield x |> string |> int  
                } |> Seq.rev |> List.ofSeq
        
            Expect.sequenceEqual (convertNumToSeq 342) (seq [2; 4; 3]) ""

            let addTwoNumber (lst1: int list) (lst2: int list) = 
                let l1 = convertListToNumber lst1
                let l2 = convertListToNumber lst2 
                convertNumToSeq (l1 + l2)

            Expect.equal (addTwoNumber ([2; 4; 3]) ([5; 6; 4])) ([7; 0; 8]) ""
            Expect.isTrue true "Median"

                
let test03 = 
    testCase "03: Longest Substring Without Repeating Characters"
    <| fun _ -> 
        let lengthOfLongestSubstring (s: string) = 
            let helper s = 
                let mutable currSet = Set.empty 
                seq {
                    for i in s do 
                        if currSet.Contains i then 
                            yield currSet 
                            currSet <- Set.empty
                            currSet <- currSet.Add i  
                        else 
                            currSet <- currSet.Add i 
                    yield currSet
                }
            helper s |> Seq.map (fun x -> Seq.length x) |> Seq.max

             

        Expect.equal (lengthOfLongestSubstring "abcabcbb") 3 "abcabcbb"
        Expect.equal (lengthOfLongestSubstring "bbbb") 1 "bbbb"
        Expect.equal (lengthOfLongestSubstring "pwwkew") 3 "pwwkew"
        Expect.isTrue true "Median"
                   

let test04 = 
    testCase "04: Median of Two Sorted Arrays"
    <| fun _ -> 
        let findMedianSortedArrays (num1: int list) (num2: int list) = 
            let merged = (num1 @ num2) |> List.sort 
            let beforeMiddle = 
                merged 
                |> List.indexed 
                |> List.takeWhile (fun (i, _) -> i < merged.Length / 2 + 1 ) 
                |> Array.ofList
                |> Array.map (fun (_, v) -> v)
                |> Array.rev

            printfn "%A" beforeMiddle

            let n = beforeMiddle.Length
            if n % 2 = 0 then 
                beforeMiddle[0] |> float
            else 
                ((float beforeMiddle[0]) + (float beforeMiddle[1])) / 2.0 
            
        Expect.equal (findMedianSortedArrays [1; 3] [2]) 2.0 "case01"
        Expect.equal (findMedianSortedArrays [1; 2] [3; 4]) 2.5 "case02"
        Expect.isTrue true "Hard"

[<Tests>]
let tests = testList "1-10" [ test01; test02; test03; test04 ]
