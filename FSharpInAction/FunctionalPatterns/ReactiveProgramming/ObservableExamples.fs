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


    module SimulateEvents = 
        // We want to create multiple subscriber 
        type ITradeObserver =
            abstract member processTradeEvent: TradeEvent -> unit 
            abstract member id: string with get

        type TradeBotV1 (id: string) = 
            let _id = id
            interface ITradeObserver with 
                member x.id = id  
                member x.processTradeEvent event = 
                    processEvent event $"got processed by TradeBot {id}"

            member x.BotId = _id 

        type TradeBotV2 (id: string) =
            interface ITradeObserver with 
                member x.id = id               
                member x.processTradeEvent event = 
                    processEvent event $"got processed by TradeBot {id}"
        
        type Topic = string

        type PubSubMsg = 
            | Subscribe of topic: Topic * observer: ITradeObserver * chnl: AsyncReplyChannel<Result<bool, string>>
            | Broadcast of topic: Topic * event: TradeEvent


        type PubSub() = 
            let subscribers: Map<Topic, Map<string, ITradeObserver>> = 
                Map.empty


            let agent =
                MailboxProcessor<PubSubMsg>.Start(fun inbox ->
                    let rec loop(subscribers: Map<Topic, Map<string, ITradeObserver>>) =
                        async {
                            let! msg = inbox.Receive()
                            match msg with 
                            | Subscribe (topic, tradeObserver, chnl) -> 
                                let subscribers' = 
                                    match Map.tryFind topic subscribers with 
                                    | None -> 
                                        Map.empty
                                        |> Map.add tradeObserver.id tradeObserver
                                        |> fun observers' -> subscribers.Add(topic, observers')
                                        //let observers: Map<string, ITradeObserver> = 
                                        //    Map.empty
                                        //    |> Map.add tradeObserver.id tradeObserver
                                        //subscribers
                                        //|> Map.add topic observers
                                    | Some observers -> 
                                        observers
                                        |> Map.add tradeObserver.id tradeObserver
                                        |> fun observers' -> subscribers.Add(topic, observers')
                                        //let observers' = 
                                        //    observers
                                        //    |> Map.add tradeObserver.id tradeObserver
                                        //subscribers
                                        //|> Map.add topic observers'
                                chnl.Reply (Ok true)
                                return! loop(subscribers')

                            | Broadcast (topic, event) ->
                                match Map.tryFind topic subscribers with 
                                | None -> ()
                                | Some observers -> 
                                    observers
                                    |> Map.iter (fun _key (observer: ITradeObserver) -> 
                                        observer.processTradeEvent event 
                                    )
                                return! loop(subscribers)
                        }
                    loop (subscribers)
                )

            member x.Subscribe topic observer = 
                async {
                    return! agent.PostAndAsyncReply(fun chnl -> Subscribe (topic, observer, chnl))
                }

            member x.Broadcast topic event = 
                agent.Post (Broadcast (topic, event))


        let pubSub = new PubSub() 
        let someSymbol = "SYMBOLX"

        let rec loop () =
            
            async {          
                do! Async.Sleep 5_000
                // Suppose we receive tradeEvent from other events
                let tradeEvent = {Size = 100}
                printfn "\n"
                pubSub.Broadcast someSymbol tradeEvent
                //processEvent tradeEvent "from other event"
                return! loop ()
            }

        let demo () = 
            let trader1 = TradeBotV1 "bot1"
            let trader2: ITradeObserver = TradeBotV2 "bot2"
            let trader3: ITradeObserver = TradeBotV1 "bot3"
            let trader4: ITradeObserver = TradeBotV2 "bot4"

            pubSub.Subscribe someSymbol (trader1: ITradeObserver) |> Async.RunSynchronously |> fun m -> $"{trader1.BotId} subscribed: {m.ToString()}" |> printfn "%s"
            pubSub.Subscribe someSymbol (trader1: ITradeObserver) |> Async.RunSynchronously |> sprintf "%A" |> fun m -> $"{trader1.BotId} subscribed: {m.ToString()}" |> printfn "%s"
            pubSub.Subscribe someSymbol trader2 |> Async.RunSynchronously |> fun m -> $"{trader2.id} subscribed: {m}" |> printfn "%A"
            pubSub.Subscribe "notExistTopic" trader3 |> Async.RunSynchronously |> fun m -> $"{trader3.id} subscribed: {m}" |> printfn "%A"
            pubSub.Subscribe someSymbol trader4 |> Async.RunSynchronously |> fun m -> $"{trader4.id} subscribed: {m}" |> printfn "%A"

            loop () |> Async.RunSynchronously




