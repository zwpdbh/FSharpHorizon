namespace Others.WorkflowCore

open WorkflowCore.Models
open WorkflowCore.Interface
open Microsoft.Extensions.DependencyInjection


// The biggest problem for adopting WorkflowCore in F# is it is C# oriented.
// The obstacle is to understand:
// https://learn.microsoft.com/en-us/dotnet/core/extensions/dependency-injection-usage

module Simple =
  module HelloWorld = 
    // HelloWorld example from: https://workflow-core.readthedocs.io/en/latest/getting-started/#steps

    type HellowWorld() =
      inherit StepBody()

      override x.Run(context: IStepExecutionContext ) =
        printfn "Hello World"
        ExecutionResult.Next()

    type GoodbyeWorld() =
      inherit StepBody()

      override x.Run(context: IStepExecutionContext) =
        printfn "Goodbye World"
        ExecutionResult.Next()

    type HelloWorldWorkflow() =
      // Implementing Interfaces by Using Class Types: https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/interfaces#implementing-interfaces-by-using-class-types
      interface IWorkflow with
        member this.Id: string = 
          "Hello World"
        member this.Version: int = 
          1

        member x.Build builder =
          builder
            .StartWith<HellowWorld>()
            .Then<GoodbyeWorld>()
            |> ignore 

    // How to define static class?
    // https://stackoverflow.com/questions/13101995/defining-static-classes-in-f
    // For get an instance of IServiceProvider
    [<AbstractClass; Sealed>]
    type ServiceProviderFactory() =
      static member ServiceProviderFactory() =
        new Microsoft.Extensions.DependencyInjection.ServiceCollection()
        



    let configureServices (services: IServiceCollection) =
      services.AddWorkflow()


    let demoHelloWorld() =
      let serviceCollection = new Microsoft.Extensions.DependencyInjection.ServiceCollection()
      serviceCollection.AddWorkflow()


      //Service

      //let host = serviceProvider.GetService<IWorkflowHost>()
      //host.RegisterWorkflow<HelloWorldWorkflow>
      //host.Start()
      
      //host.StartWorkflow("HelloWorld", 1, null)
      
      //Console.ReadLine();
      //host.Stop()
