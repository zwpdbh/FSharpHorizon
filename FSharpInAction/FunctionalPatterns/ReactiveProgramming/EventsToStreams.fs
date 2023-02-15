namespace FunctionalPatterns.ReactiveProgramming

module EventsToStreams =
    // See: https://fsharpforfunandprofit.com/posts/concurrency-reactive/

    module SimpleEventSystem = 
        // classic event handler approach
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

        let demo () = 
            // create a handler. The event args are ignored
            let basicHandler _ = printfn "tick %A" DateTime.Now
            
            // register the handler
            let basicTimer1 = createTimer 1000 basicHandler
            
            // run the task now
            Async.RunSynchronously basicTimer1

