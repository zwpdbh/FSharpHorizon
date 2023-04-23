// For more information see https://aka.ms/fsharp-console-apps
namespace SimpleWorkflow

module SimpleWorkflowMain =

  [<EntryPoint>]
  let main argv =
    //HellowWorkflow.demo() |> printfn "%A"
    HellowSaga.demo() |> printfn "%A"
      
    0 



