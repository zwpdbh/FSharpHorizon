namespace FunAndProfit

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