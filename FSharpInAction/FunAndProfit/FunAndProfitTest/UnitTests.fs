module Tests

open Expecto

open FunAndProfit
open FunAndProfit.ExpressionSimplification

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

let test02 = 
    testCase "02 Test simple derivative"
    <| fun _ -> 
        
        let e1 = Sum (Num 1, Prod (Num 2, Var))
        let e2 = deriv e1 

        Expect.equal e2 (Sum (Num 0,Sum (Prod (Num 2,Num 1),Prod (Var,Num 0)))) ""

        let e3 = Prod (Var, Prod (Var, Num 2))
        let printedResult = (deriv e3).ToString()
        Expect.equal printedResult "x*(x*0+2*1)+x*2*1" ""
        
        Expect.equal ((simpDeriv e3).ToString()) "x*2+x*2" ""


[<Tests>]
let tests =
  testList "UnitTests" [
    test01
    test02
  ]
