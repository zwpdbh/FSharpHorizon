namespace FunctionalPatterns.ReactiveProgramming

module EventsToStreams =
    // See: https://fsharpforfunandprofit.com/posts/concurrency-reactive/
    open System
    open System.Threading

    let createTimer timerInterval eventHandler =
        let timer = new System.Timers.Timer(float timerInterval)
        timer.AutoReset <- true
        timer.Elapsed.Add eventHandler

        async {
            timer.Start()
            do! Async.Sleep 5000
            timer.Stop()
        }

    let createTimerAndObservable timerInterval =
        // setup a timer
        let timer = new System.Timers.Timer(float timerInterval)
        timer.AutoReset <- true

        // events are automatically IObservable
        let observable = timer.Elapsed

        // return an async task
        let task =
            async {
                timer.Start()
                do! Async.Sleep 5000
                timer.Stop()
            }

        // return a async task and the observable
        (task, observable)

    module SimpleEventSystem =
        // classic event handler approach

        let demoRegisterHandler () =
            // create a handler. The event args are ignored
            let basicHandler _eventArgs = printfn "tick %A" DateTime.Now

            // register the handler
            let basicTimer1 = createTimer 1000 basicHandler

            // run the task now
            Async.RunSynchronously basicTimer1

        /// Differet from demoRegisterHandler by exposing the event stream outside to be subscribed
        let demoSubscribeEventStream () =

            // create the timer and the corresponding observable
            let basicTimer2, timerEventStream = createTimerAndObservable 1000

            // register that every time something happens on the
            // event stream, print the time.
            timerEventStream
            |> Observable.subscribe (fun _ -> printfn "tick %A" DateTime.Now)
            |> ignore

            // run the task now
            Async.RunSynchronously basicTimer2


    module CountEvent =
        //In this next example, we’ll have a slightly more complex requirement:
        //Create a timer that ticks every 500ms.
        //At each tick, print the number of ticks so far and the current time.

        type ImperativeTimerCount() =
            let mutable count = 0

            // the event handler. The event args are ignored
            member this.handleEvent _ =
                count <- count + 1
                printfn "timer ticked with count %i" count

        /// A classic imperative way
        let demoUseHandler () =
            // create a handler class
            let handler = new ImperativeTimerCount()

            // register the handler method
            let timerCount1 = createTimer 500 handler.handleEvent

            // run the task now
            Async.RunSynchronously timerCount1


        let demoSubscribeEventStream () =
            // create the timer and the corresponding observable
            let timerCount2, timerEventStream = createTimerAndObservable 500

            // set up the transformations on the event stream
            timerEventStream
            |> Observable.scan (fun count _ -> count + 1) 0
            |> Observable.subscribe (fun count -> printfn "timer ticked with count %i" count)
            |> ignore

            // run the task now
            Async.RunSynchronously timerCount2


    module MergingMultipleEvents =
        // Create two timers, called '3' and '5'. The '3' timer ticks every 300ms and the '5' timer ticks every 500ms
        // Handle the events as follows:
        // a) for all events, print the id of the time and the time
        // b) when a tick is simultaneous with a previous tick, print 'FizzBuzz'
        // otherwise:
        // c) when the '3' timer ticks on its own, print 'Fizz'
        // d) when the '5' timer ticks on its own, print 'Buzz'

        type FizzBuzzEvent = { label: int; time: DateTime }

        let areSimultaneous (earlierEvent, laterEvent) =
            let { label = _; time = t1 } = earlierEvent
            let { label = _; time = t2 } = laterEvent
            t2.Subtract(t1).Milliseconds < 50

        type ImperativeFizzBuzzHandler () =

            let mutable previousEvent: FizzBuzzEvent option = None

            let printEvent thisEvent =
                let { label = id; time = t } = thisEvent
                printf "[%i] %i.%03i " id t.Second t.Millisecond

                let simultaneous =
                    previousEvent.IsSome
                    && areSimultaneous (previousEvent.Value, thisEvent)

                if simultaneous then printfn "FizzBuzz"
                elif id = 3 then printfn "Fizz"
                elif id = 5 then printfn "Buzz"

            member this.handleEvent3 eventArgs =
                let event = { label = 3; time = DateTime.Now }
                printEvent event
                previousEvent <- Some event

            member this.handleEvent5 eventArgs =
                let event = { label = 5; time = DateTime.Now }
                printEvent event
                previousEvent <- Some event

        let demoImperativeOne () = 
            // create the class
            let handler = new ImperativeFizzBuzzHandler()
            
            // create the two timers and register the two handlers
            let timer3 = createTimer 300 handler.handleEvent3
            let timer5 = createTimer 500 handler.handleEvent5
            
            // run the two timers at the same time
            [timer3; timer5]
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore

        let demoFunctionalOne () = 
            let timer3, timerEventStream3 = createTimerAndObservable 300
            let timer5, timerEventStream5 = createTimerAndObservable 500

            // convert the time events into FizzBuzz events with the appropriate id
            let eventStream3  =
               timerEventStream3
               |> Observable.map (fun _ -> {label=3; time=DateTime.Now})
            
            let eventStream5  =
               timerEventStream5
               |> Observable.map (fun _ -> {label=5; time=DateTime.Now})


            //We have started with the two original event streams and from them created four new ones:
            //combinedStream contains all the events
            //simultaneousStream contains only the simultaneous events
            //fizzStream contains only the non-simultaneous events with id=3
            //buzzStream contains only the non-simultaneous events with id=5

            // combine the two streams
            let combinedStream =
                Observable.merge eventStream3 eventStream5
            
            // split the stream based on whether the pairs are simultaneous
            let simultaneousStream, nonSimultaneousStream =
                // make pairs of events
                let pairwiseStream =
                   combinedStream |> Observable.pairwise

                pairwiseStream |> Observable.partition areSimultaneous

            // split the non-simultaneous stream based on the id
            let fizzStream, buzzStream  =
                nonSimultaneousStream
                // convert pair of events to the first event
                |> Observable.map (fun (ev1,_) -> ev1)
                // split on whether the event id is three
                |> Observable.partition (fun {label=id} -> id=3)

            // Now, attach behavior to each stream
            //print events from the combinedStream
            combinedStream
            |> Observable.subscribe (fun {label=id;time=t} ->
                                          printf "[%i] %i.%03i " id t.Second t.Millisecond) |> ignore
            
            //print events from the simultaneous stream
            simultaneousStream
            |> Observable.subscribe (fun _ -> printfn "FizzBuzz") |> ignore
            
            //print events from the nonSimultaneous streams
            fizzStream
            |> Observable.subscribe (fun _ -> printfn "Fizz") |> ignore
            
            buzzStream
            |> Observable.subscribe (fun _ -> printfn "Buzz") |> ignore


            // Run the timer to trigger
            [timer3;timer5]
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore