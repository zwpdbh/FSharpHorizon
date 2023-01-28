namespace FunAndProfit

open Expecto

module Say =
    let hello name =
        printfn "Hello %s" name


// From https://www.daniellittle.dev/dynamically-calling-a-function-in-fsharp
module Dynamic = 
    open System
    open FSharp.Reflection

    type InvokeResult = 
        | Success of obj
        | ObjectWasNotAFunction of Type

    let Call (fn: obj) (args: obj seq) = 
        let rec dynamicFunctionInternal (next: obj) (args: obj list) : InvokeResult = 
            match args.IsEmpty with 
            | false -> 
                let fType = next.GetType()
                if FSharpType.IsFunction fType then 
                    let (head, tail) = (args.Head, args.Tail)
                    let methodInfo = 
                        fType.GetMethods()
                        |> Seq.filter (fun x -> x.Name = "Invoke" && x.GetParameters().Length = 1)
                        |> Seq.head 
                    let partialResult = methodInfo.Invoke(next, [|head|])
                    dynamicFunctionInternal partialResult tail 
                else 
                    ObjectWasNotAFunction fType
            | true -> 
                Success(next)

        dynamicFunctionInternal fn (args |> List.ofSeq)

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

            let testResult = Call mySum [[1;2;3]; [1;2;3;100]]     
            match testResult with 
            | Success obj -> 
                Expect.equal (obj :?> int)  114 ""
            | _ -> 
                0 |> ignore  

    [<Tests>]
    let tests =
        testList "Dynamic call function" [
            test01
        ]