
open DemoREPerformance

//let n = 5000 * 5

//timeRegulareExpression n |> printfn "regulare expression takes: %A" 
//timeStringContain n |> printfn "string contains takes: %A" 

//OtherTopics.BinaryFileReader.demo01() |> ignore
//OtherTopics.Database.Others.demo () |> ignore

//Puzzles.Others.Turing01.scores ["5"; "2"; "C"; "D"; "+"] |> printfn "%A" // 30 
//Puzzles.Others.Turing01.scores ["5"; "-2"; "4"; "C"; "D"; "9"; "+"; "+"] |> printfn "%A" // 27

Puzzles.Others.Turing01.isValidParentheses "()[]{}" |> printfn "%A" // true
Puzzles.Others.Turing01.isValidParentheses "([)]" |> printfn "%A" // false
