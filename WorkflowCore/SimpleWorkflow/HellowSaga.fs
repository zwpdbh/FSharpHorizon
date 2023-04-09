namespace SimpleWorkflow

open Microsoft.Extensions.DependencyInjection
open WorkflowCore.Interface
open WorkflowCore.Models
open System
open HellowWorkflow.Steps

module HellowSaga =
  

  type HelloSagaWorkflow() =
    interface IWorkflow with
      member this.Id: string =
        "HelloSagaWorkflow"
      member this.Version: int =
        1

      member x.Build builder =
        builder
          .StartWith(fun context -> printfn "Start")
          .Saga(fun saga ->
            saga
              .StartWith<Initialize>()
                .CompensateWith<UndoInitialize>()
              .Then<ApplyDiscount>()
                .CompensateWith<UndoApplyDiscount>()
              |> ignore 
          )
          .CompensateWith<Cleanup>()
          .Then(fun context -> printfn "End")
          |> ignore 

  let demo() =
    let serviceProvider =
      (new ServiceCollection())
        .AddLogging()
        .AddWorkflow()
        .BuildServiceProvider()

    let host = serviceProvider.GetService<IWorkflowHost>()
    if (host = null) then
      failwith "Host is not initialized"

    host.RegisterWorkflow<HelloSagaWorkflow>()
    host.Start()

    let runWorkflow() =
      async {
        return! host.StartWorkflow("HelloSagaWorkflow") |> Async.AwaitTask
      }
      |> Async.RunSynchronously
      |> ignore

    runWorkflow()
    System.Console.ReadLine() |> ignore
    host.Stop() 



