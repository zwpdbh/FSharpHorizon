namespace SimpleWorkflow

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open WorkflowCore.Interface
open WorkflowCore.Models

module HellowWorkflow =
  module Steps =
    //Initialize
    //ApplyDiscount
    //ApplyShipping
    //Finalize
    type Initialize() =
      inherit StepBody()
      override x.Run(context: IStepExecutionContext) =
        printfn "Initialize"
        ExecutionResult.Next()

    type ApplyDiscount() =
      inherit StepBody()
      override x.Run(context: IStepExecutionContext) =
        printfn "ApplyDiscount"
        ExecutionResult.Next()

    type ApplyShipping() =
      inherit StepBody()
      override x.Run(context: IStepExecutionContext) =
        printfn "ApplyShipping"
        ExecutionResult.Next()

    type Finalize() =
      inherit StepBody()
      override x.Run(context: IStepExecutionContext) =
        printfn "Finalize"
        ExecutionResult.Next()

  module Workflow =
    open Steps 
    type HelloWorldWorkflow() =
      // Implementing Interfaces by Using Class Types: https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/interfaces#implementing-interfaces-by-using-class-types
      interface IWorkflow with
        member this.Id: string = 
           "ProcessPaymentWorkflow";
        member this.Version: int = 
          1

        member x.Build builder =
          builder
            .UseDefaultErrorBehavior(WorkflowErrorHandling.Suspend)
            .StartWith<Initialize>()
            .Then<ApplyDiscount>()
            .Then<ApplyShipping>()
            .Then<Finalize>()
            |> ignore

  let demo () = 
    let serviceProvider =
      (new ServiceCollection())
        .AddLogging()
        .AddWorkflow()
        .BuildServiceProvider()
    let host = serviceProvider.GetService<IWorkflowHost>()
    if (host = null) then
      failwith "Host is not initialized"

    host.RegisterWorkflow<Workflow.HelloWorldWorkflow>()
    host.Start()

    let runWorkflow() =
      async {
        return! host.StartWorkflow("ProcessPaymentWorkflow") |> Async.AwaitTask
      }
      |> Async.RunSynchronously
      |> ignore

    runWorkflow()
    System.Console.ReadLine() |> ignore
    host.Stop() 

