namespace WhyFSharp

module Demo01 =
    // Eliminate duplicates
    // Shows pattern matching is powerful
    // Shows it is generic by default
    // Shows it doesn't accept null
    // Shows it prefer recursive over loop 
    let rec compress =
        function
        | a :: (b :: _ as tail) ->
            // Instead of keeping the first and ignore following repeated as I first though
            // It continue to see next until meet different element to keep the last one !
            if a = b then
                compress tail
            else
                a :: (compress tail)
        | x -> x

    let demo01 () =
        compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]

    let demo02 () = 
        compress [1; 1; 2; 2; 3; 3; 3; 3; 4]

    let add x y = 
        x + y  

    let add10  = add 10
    //add10 20 


module Demo02 = 
    // Shows the pipeline operator
    // Shows the lambda 
    // Shows again loop like for i = 0; i <= 100; i++ is barely used in F#
    // The point is F# (FP) focus on the transformation of input to output
    let demo() = 
        let fizzBuzz x = 
            match x with 
            | _ when x % 3 = 0 && x % 5 = 0 -> 
                "FizzBuzz"
            | _ when x % 3 = 0 -> 
                "Fizz"
            | _ when x % 5 = 0 -> 
                "Buzz"
            | _ ->
                $"{x}"

        [1..100]
        |> List.iter (fun x -> fizzBuzz x |> printfn "%A")
        
module Demo03 = 
    // Shows DU used as inheritance. Inheritance usually can be considered as composition
    // Shows explict convert from int to float
    // Show they are just functions
    type Point = {x: int; y: int}

    type Shape = 
        | Rectangle of width: int * height: int * point: Point
        | Square of edge: int * point: Point
        | Circle of radius: int * point: Point

        member this.area = 
            match this with 
            | Rectangle (x, y, _) -> float (x * y )
            | Square (e, _) -> float (e * e)
            | Circle (r, _) -> 3.14 * float r * float r 

    let rectangle = Rectangle (20, 10, {x = 1; y = 2})
    let squre = Square (10, {x = 0; y = 0})
    let circle = Circle (10, {x = 0; y = 2})

    let demo() = 
        [rectangle; squre; circle]
        |> List.map (fun x -> x.area)
        |> List.sum


module Demo04 = 
    // Shows DU as better domain modeling
    type Player = {name: string; score: int}

    type Game = 
        | NotStarted
        | InProcess of Player * Player 
        | Finished of Player

    let gameStatus (game: Game) = 
        match game with 
        | NotStarted -> "Game not started: Waiting for players to join"
        | InProcess (player01, player02) -> 
            $"Game is on: {player01} vs {player02}"
        | Finished player -> 
            $"Game is finished: {player} is the winner!"

    let demo() = 
        let game01 = NotStarted
        let game02 = InProcess ({name = "player01"; score = 0}, {name = "player02"; score = 0})
        let game03 = Finished {name = "player01"; score = 100}
        printfn $"{gameStatus game01}"
        printfn $"{gameStatus game02}"
        printfn $"{gameStatus game03}"

        

/// Something F# feature that is hard to achieve in C#
// What is active pattern?
// “Active patterns” means the pattern can be parsed or detected dynamically like it is a normal value.
// Active pattern 01
module Demo05 = 
    type Temperature =
    | Celsius of float
    | Fahrenheit of int

    // Define an active pattern: IsWarm | IsCold
    let (|IsWarm|IsCold|) temperature = 
        match temperature with 
        | Celsius c when c > 25.0 -> IsWarm
        | Celsius _ -> IsCold
        | Fahrenheit f when f > 77 -> IsWarm
        | Fahrenheit _ -> IsCold

    // Use it dynamically: the active pattern "IsWarm | IsCold" 
    // is matched dynamically in pattern matching with Temperature
    // Active pattern here hide the implementation details about how to decide warm or code for a Temperature.
    let isItWarm temperature = 
        match temperature with 
        | IsWarm -> true 
        | IsCold -> false

    let demo () = 
        isItWarm (Celsius 32.0) |> printfn "%A"
        isItWarm (Fahrenheit 88) |> printfn "%A"


// Active pattern 02
module Demo06 = 
    // Maybe the AdventOfCode2022 Day06 is a good example?
    open System.Text.RegularExpressions

    // Here, we defined FirstRegexGroup as partial active pattern 
    // which could return Some value or None.
    let (|FirstRegexGroup|_|) pattern input = 
            let m = Regex.Match(input, pattern)
            if (m.Success) then Some m.Groups.[1].Value else None 

    // Here, in this function we pattern matching input with different pattern dynamically
    // and based on result (Some value or None) we do further processing.
    let extractHost str = 
        match str with 
        | FirstRegexGroup "http://(.*?)/(.*)" host -> 
            host 
        | FirstRegexGroup ".*?@(.*)" host -> 
            host
        | FirstRegexGroup "It will be None, therefore not matched" host -> 
            host
        | _ -> str

    let demo() = 
        extractHost "http://google.com/test" |> printfn "%A"
        extractHost "alice@hotmail.com" |> printfn "%A"
        extractHost "unknown" |> printfn "%A"


/// Computation Expression
// It is very powerful, not possible in C#
// Computation expressions “hide” handling of async, errors and more
// Model: Customer with an optional Name, Data with an Amount
// Input: Customer ID and Data ID
// 1. Load Customer by its ID
// 2. Load Data by its ID
// 3. Get the Name of the Customer (if the Customer was found)
// 4. Get the Amount of the Data (if the Data was found)
// 5. Return a tuple with the Name and Amount if all went well, otherwise return some kind of error
module Demo07 = 
    open FsToolkit.ErrorHandling
    // See: https://demystifyfp.gitbook.io/fstoolkit-errorhandling
    // READ LATER: https://fsharpforfunandprofit.com/series/map-and-bind-and-apply-oh-my/

    type Customer = {Id: int; Name: string option}
    type Data = {Id: int; Amount: int}

    // simulate loading customer, only succeed when customerId is 42
    let loadCustomer customerId = 
        asyncResult {
            do! customerId = 42
                |> Result.requireTrue 
                    "customer not found"

            return {Id = customerId; Name = Some "Charles"}
        }

    let loadData dataId = 
        asyncResult {
            return {Id = dataId; Amount = 100}
        }

    let getNameOfCustomer customer = 
        option {
            let! name = customer.Name 
            return name
        }

    // Simplifies reading, understanding and writing code
    let getCustomerNameAndAmount customerId dataId  = 
        asyncResult {
            // Has a value when the customer could be loaded, 
            // otherwise the whole computation expression returns the error.
            // Kind of an early return.
            let! customer = loadCustomer customerId
            let! data = loadData dataId 

            let! name = 
                customer 
                |> getNameOfCustomer
                |> Result.requireSome "customer has no name"

            return name, data.Amount
        }

    let compute () = 
        async {
            let! result = getCustomerNameAndAmount 42 17
            match result with 
            | Ok (name, amount) -> printf $"customer = {name}, amount = {amount}"
            | Error error -> printf $"error is {error}"
        }

    let demo () = 
        compute () |> Async.RunSynchronously


/// MessagePassing as better way to handle concurrency
// This demo shows 
// 1. Spawn multiple workers (agent) to compute multiple tasks 
// 2.1 A worker request new task from scheduler if it is not busy
// 2.2 A worker report its task result back to scheduler.
// 3. Scheduler assign new task to worker if there is till work to do 
// otherwise it tolds worker to shutdown
// In this example, the scheduler assign 30 tasks (they all compute Fib(40) to 
// multiple workers and collect their result.
module Demo08 =
    open System.Collections.Generic

    type SchedulerMessage =
        | Ready of int * AsyncReplyChannel<WorkerMessage>
        | Answer of int * int
        | Process of AsyncReplyChannel<int>
        | Summary of AsyncReplyChannel<(int * int) list>

    and WorkerMessage =
        | Fib of int
        | Shutdown

    let rec fibCalc n =
        match n with
        | 0 -> 0
        | 1 -> 1
        | n -> fibCalc (n - 1) + fibCalc (n - 2)


    let rec schedulerProcess(jobs:Queue<int>) =
        MailboxProcessor<SchedulerMessage>.Start
            (fun inbox ->
                // Scheduler is more passively to wait for events happen
                // It receive message and pass response back to client through replyChannel (more like a traditional server)
                let mutable results = []
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | Ready (id, replyChannel) when jobs.Count <> 0 ->
                            let n = jobs.Dequeue()
                            replyChannel.Reply(Fib n)
                            return! loop ()
                        | Ready (_, replyChannel) ->
                            replyChannel.Reply(Shutdown)
                            return! loop ()
                        | Answer (num, result) ->
                            results <- [ (num, result) ] @ results
                            return! loop ()
                        | Process replyChannel -> 
                            replyChannel.Reply(results.Length)
                            return! loop ()
                        | Summary replyChannel -> replyChannel.Reply results
                    }

                loop ())

    // Anyway to create mutualy exclusive functions accross modules?? https://stackoverflow.com/questions/2904889/f-mutual-recursion-between-modules
    and workerProcess (id: int) (scheduler: MailboxProcessor<SchedulerMessage>)  =
        MailboxProcessor<WorkerMessage>.Start
            (fun inbox ->
                // It seems Erlang's symmetric send and received is difficult to achieve.
                // There, worker is the more active one, it use schedule's reference to send message to it and receive its response.
                let rec loop () =
                    async {
                        let! msg = scheduler.PostAndAsyncReply(fun reply -> Ready(id, reply))

                        match msg with
                        | Shutdown ->
                            ()
                        | Fib n ->
                            let fibn = fibCalc (n)
                            scheduler.Post(Answer(n, fibn))
                            return! loop ()
                    }

                loop ())

    let computeFibs nworkers nJobs fibX=
        // printfn $"use {nworkers} workers to compute {nJobs} of fib({fibX})"
        let jobs =
            let lst = 
                seq {
                    for _ in [1..nJobs] do 
                        yield fibX
                }
            new Queue<int>(lst)

        let scheduler = schedulerProcess(jobs)
   
        [ 1..nworkers ]
        |> List.map (fun i -> workerProcess i scheduler)
        |> ignore

        let mutable finishedJobs = 0
        async {     
            while finishedJobs <> nJobs do 
                let! updated = scheduler.PostAndAsyncReply(fun reply -> Process reply)
                if updated <> finishedJobs then 
                    finishedJobs <- updated
            return! scheduler.PostAndAsyncReply(fun reply -> Summary(reply)) }
        |> Async.RunSynchronously
        |> ignore

    let duration f =
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        f ()
        timer.ElapsedMilliseconds

    // measure the time using n workers to do m tasks in which each task compute fib(x)
    let measureNworkers n m x = 
        let cost = duration (fun _ -> computeFibs n m x)
        printfn $"{n} workers tooks: {cost}ms"    

    // Print out the time used for using different number of workers, something like:
    //    > 
    //5 workers tooks: 4112ms
    //6 workers tooks: 3411ms
    //7 workers tooks: 3345ms
    //8 workers tooks: 2855ms
    //9 workers tooks: 2757ms
    //...
    let demo () =
        [5 .. 20] 
        |> List.iter(fun n -> 
            measureNworkers n 30 40
        )    

module DemoAsycSeq = 
    // An AsyncSeq is a sequence in which individual elements are retrieved using an Async computation.
    open FSharp.Control
    open System.IO

    let someCompute x = 
        async {
            printfn $"\n compute {x}"
            do! Async.Sleep(2 * 1000)
            return Some x 
        }

    let asyncMap f computation =
        async {
            let! x = computation
            return f x
        }

    let duration f =
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        f () |> ignore
        timer.ElapsedMilliseconds
        
    let useAsyncPallel (input: int list) = 
        input
        |> List.map someCompute 
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.choose id 
        |> Array.sum

    let useAsyncSequantial (input: int list) = 
        input
        |> List.map someCompute 
        |> Async.Sequential
        |> asyncMap (Array.choose id)
        |> Async.RunSynchronously
        |> Array.sum

    let useAsyncSeqV1 (input: int list) = 
        // This will execute one by one with order
        input
        |> AsyncSeq.ofSeq
        |> AsyncSeq.mapAsync someCompute
        |> AsyncSeq.toListAsync
        |> Async.RunSynchronously
        |> List.choose id 
        |> List.sum

    let useAsyncSeqV2 (input: int list) = 
        // This will execute one by one with order
        input
        |> AsyncSeq.ofSeq
        |> AsyncSeq.mapAsync someCompute
        |> AsyncSeq.toListAsync
        |> asyncMap (List.choose id)
        |> Async.RunSynchronously
        |> List.sum

    let useAsyncSeqV3 (input: int list) = 
        // This will execute one by one with order
        input
        |> AsyncSeq.ofSeq
        |> AsyncSeq.mapAsyncParallel someCompute
        |> AsyncSeq.toListAsync
        |> asyncMap (List.choose id)
        |> Async.RunSynchronously
        |> List.sum 

    let useAsyncSeqV4 (input: int list) = 
        // This will execute one by one with order
        input
        |> AsyncSeq.ofSeq
        |> AsyncSeq.mapAsync someCompute
        |> AsyncSeq.choose id 
        |> AsyncSeq.sum
        |> Async.RunSynchronously

    let useAsyncSeqV5 (input: int list) = 
        // This will execute one by one with order
        input
        |> AsyncSeq.ofSeq
        |> AsyncSeq.mapAsync someCompute
        |> AsyncSeq.chooseAsync (fun x -> 
            async {
                return id x
            }
        )
        |> AsyncSeq.toBlockingSeq
        |> Seq.sum
        
    let useAsyncSeqV6 (input: int list) = 
        // This will execute one by one with order
        input
        |> AsyncSeq.ofSeq
        |> AsyncSeq.mapAsync someCompute
        |> AsyncSeq.chooseAsync (fun x -> 
            async {
                return id x
            }
        )
        |> AsyncSeq.sum
        |> Async.RunSynchronously


    let demo () = 
        [
            //useAsyncPallel
            //useAsyncSeqV1
            //useAsyncSeqV2
            //useAsyncSeqV3
            //useAsyncSequantial
            //useAsyncSeqV4
            //useAsyncSeqV5
            useAsyncSeqV6
        ]
        |> List.indexed
        |> List.map (fun (i, f) ->
            let result = duration (fun _ -> f [1..20])
            printfn $"===Case {i} took {result} ms===\n\n\n"
        )
        |> ignore

