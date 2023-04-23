namespace SimpleWorkflow

open Microsoft.Extensions.DependencyInjection
open WorkflowCore.Interface
open WorkflowCore.Models

module PassingDataWorkflow =
  type AddNumbers() =
    inherit StepBody()
    member x.Input1 = 0
    member x.Input2 = 0
    member x.Output = x.Input1 + x.Input2
    override x.Run(context: IStepExecutionContext) =
      ExecutionResult.Next()

  type MyData()=
    // See: https://stackoverflow.com/questions/24840948/when-should-i-use-let-member-val-and-member-this
    member val Value1 = 0 with get, set
    member x.Value2 = 0