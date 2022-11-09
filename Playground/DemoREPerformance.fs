module DemoREPerformance

open System.Diagnostics
open System.Text.RegularExpressions

let failureActivityMessage = """
    But as we know, event handling, like concurrency in general, can be tricky to implement. 
    Simple event logic is straightforward, but what about logic like “do something if two events happen in a row but do something different if only one event happens”
    """

let knowIssuePattern = "concurrency in general"

let timeF f repeatN = 
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    for i in seq {1 .. repeatN} do 
        f |> ignore

    stopWatch.Stop()
    stopWatch.Elapsed.TotalMilliseconds


let timeRegulareExpression n = timeF (fun _ -> Regex.IsMatch(failureActivityMessage, knowIssuePattern)) n
let timeStringContain n = timeF (fun _ -> failureActivityMessage.Contains(knowIssuePattern)) n

