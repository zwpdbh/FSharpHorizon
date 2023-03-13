namespace FunctionalPatterns.ReactiveProgramming

module ObservableExamples =
    open FSharp.Control.Reactive
    
    type TradeEvent = { Size: int }

    let tradeEvents01 = 
        [1..3..20]
        |> List.map (fun x -> {Size = x})
        |> Observable.ofSeq

    let tradeEvents02 = 
        [0..2..20]
        |> List.map (fun x -> {Size = x})
        |> Observable.ofSeq

    let processEvent event message = 
        printfn $"TradeEvent: Size = {event.Size}. OtherInfo: {message}"


    module MergeAndSplit = 
        let demo () =
            let totalTradeEvent = 
                Observable.merge tradeEvents01 tradeEvents02

            let fizzEvents = 
                totalTradeEvent 
                |> Observable.filter (fun x -> x.Size % 3 = 0)
                

            let BuzzEvents = 
                totalTradeEvent
                |> Observable.filter (fun x -> x.Size % 5 = 0)

            let fizzBuzzEvents = 
                totalTradeEvent
                |> Observable.filter (fun x -> x.Size % 3 = 0 && x.Size % 5 = 0)

            fizzEvents.Subscribe (fun x -> processEvent x "fizz") |> ignore 
            BuzzEvents.Subscribe (fun x -> processEvent x "Buzz") |> ignore
            fizzBuzzEvents.Subscribe (fun x -> processEvent x "fizzBuzz") |> ignore


    




