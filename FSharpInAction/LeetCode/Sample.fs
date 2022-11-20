module Tests

open Expecto

type LinkList<'T> = 
     | Empty
     | Node of value: 'T * next: LinkList<'T>

let test01 =
    testCase "01: Two Sum"
    <| fun _ -> 
        let twoSum (nums: int seq) (target: int) = 
            let numsWithIndex = Seq.indexed nums
            seq {
                for (i, x) in numsWithIndex do 
                    for (j, y) in numsWithIndex do 
                        if i < j && (x + y = target)  then 
                            yield (i, j)
            }         
        
        Expect.sequenceEqual (twoSum (seq [2; 7; 11; 15]) 9) (seq [(0, 1)]) "" 
        

[<Tests>]
let tests = testList "1-10" [ test01 ]
;