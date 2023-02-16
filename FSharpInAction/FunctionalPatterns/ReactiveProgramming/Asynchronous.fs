namespace FunctionalPatterns.ReactiveProgramming 
module Asynchronous = 
    module MyTest = 
        let heavyFunc (n: int) = 
            async {
                do! Async.Sleep n
                printfn $"use {n} ms"
                return n 
            } 

        let duration f = 
            let timer = new System.Diagnostics.Stopwatch()
            timer.Start ()
            
            f () |> printfn "%A"

            timer.Stop ()
            timer.ElapsedMilliseconds 
            
        let demoOneAfterAnother () = 
            let func () = 
                [5000; 4000; 3000; 2000; 1000]
                |> List.map (fun n -> heavyFunc n |> Async.RunSynchronously )
                |> List.sum

            duration func |> printfn "Total: use %d ms"


        let demoParallelAsync () = 
            printfn "\n"
            /// Because asynchronous computations execute independently of program flow, there is no defined order in which they print their information and finish executing.
            let func () = 
                [5000; 4000; 3000; 2000; 1000]
                |> List.map heavyFunc 
                |> Async.Parallel
                |> Async.RunSynchronously 
                |> Array.sum              

            duration func |> printfn "Total: use %d ms"


        let demoSequenceAsync () = 
            printfn "\n"
            /// each successive operation will not be scheduled until after the preceding computation has finished executing, the computations are sequenced such that there is no overlap in their execution.
            let func () = 
                [5000; 4000; 3000; 2000; 1000]
                |> List.map heavyFunc 
                |> Async.Sequential
                |> Async.RunSynchronously 
                |> Array.sum
                   
            duration func |> printfn "Total: use %d ms"
    
    /// Practise from https://fsharpforfunandprofit.com/posts/concurrency-async-and-parallel/
    module FunAndProfit = 
        open System
        open System.Threading

        let demoCancellingWorkflow () = 
            let testLoop = async {
                for i in [1..100] do
                    // do something
                    printf "%i before.." i
            
                    // sleep a bit
                    do! Async.Sleep 10
                    printfn "..after"
                }

            // create a cancellation source
            use cancellationSource = new CancellationTokenSource()
            
            // start the task, but this time pass in a cancellation token
            Async.Start (testLoop,cancellationSource.Token)
            
            // wait a bit
            Thread.Sleep(700)
            
            printfn "Cancel it after 700ms"
            cancellationSource.Cancel()


    /// Continuation Passing Style (CPS)
    /// See: https://medium.com/@dagbrattli/asynchronicity-in-f-eb4c952f0035#2daa
    module CPS = 
        let demoSyncStyle () = 
            let add a b = a + b
            let square x = x * x
            let pythagoras a b = sqrt (add (square a) (square b))
            let result = pythagoras 10.0 20.0
            result

        let demoCPSStyle () =
            let addCps a b cont : unit =
                cont (a + b)
            
            let squareCps x cont : unit =
                cont (x * x)
            
            let sqrtCps x cont : unit =
                cont (sqrt x)
            
            // Unindented Pythagoras CPS
            let pythagorasCps a b return' : unit =
                squareCps a (fun aa ->
                squareCps b (fun bb ->
                addCps aa bb (fun aabb ->
                sqrtCps aabb (fun result ->
                return' result))))

            pythagorasCps 10.0 20.0

        let demoCPSProblem () = 
            // We’re not really used to sending in a function that gets the result. We want to return the result.
            let times10 (x : int) (cont : int -> unit) : unit =
                cont (x * 10)
            
            let cb result =
                printfn "%A" result
            
            do times10 42 cb

        let demoCPSFix () = 
            // Return a function that takes a callback that takes the result.
            let times10 (x: int) : ((int -> unit) -> unit) =
                let thenDo (cb : int -> unit) =
                    cb  (x * 10)
                thenDo
            
            let cb result =
                printfn "%A" result
            
            let thenDo = times10 42
            do thenDo(cb)

    /// TBD: https://medium.com/@dagbrattli/asynchronicity-in-f-eb4c952f0035#2daa
    /// Hard to read
    module Promise = 
        let demo () = 
            0