module Tests

open Expecto

open FunAndProfit
let test01 = 
    testCase "01 Dynamic call function"
    <| fun _ -> 
        let mySum lst1 lst2 = 
            let rec aux acc lst1 lst2 = 
                match lst1, lst2 with 
                | x::rest1, y::rest2 -> 
                    aux (acc + x * y) rest1 rest2 
                | [], list2 -> acc + List.sum list2 
                | list1, [] -> acc + List.sum list1 

            aux 0 lst1 lst2 

        let testResult = Dynamic.Call mySum [[1;2;3]; [1;2;3;100]]     
        match testResult with 
        | Dynamic.Success obj -> 
            Expect.equal (obj :?> int)  114 ""
        | _ -> 
            0 |> ignore  
        

[<Tests>]
let tests =
  testList "UnitTests" [
    test01
  ]
