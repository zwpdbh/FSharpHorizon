
open DemoREPerformance
open FunAndProfit

//let n = 5000 * 5

//timeRegulareExpression n |> printfn "regulare expression takes: %A" 
//timeStringContain n |> printfn "string contains takes: %A" 

let mySum lst1 lst2 = 
    let rec aux acc lst1 lst2 = 
        match lst1, lst2 with 
        | x::rest1, y::rest2 -> 
            aux (acc + x * y) rest1 rest2 
        | [], list2 -> acc + List.sum list2 
        | list1, [] -> acc + List.sum list1 

    aux 0 lst1 lst2 

let testResult = Dynamic.Call mySum [[1;2;3]; [1;2;3;100]]
printfn "%A" testResult