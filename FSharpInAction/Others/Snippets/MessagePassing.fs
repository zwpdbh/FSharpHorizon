namespace Others.Snippets

module MessagePassing =
  open System.Collections.Generic
  open System

  type Message =
    | ShutdownSolver of string
    | ShutdownScheduler of string 
    | Fib of int
    | Ready of string * MailboxProcessor<Message> 
    | Answer of x: int * fibX: int
    | Summary of AsyncReplyChannel<bool * ((int * int) list)>


  let timeoutLimit = 5000

  module Scheduler = 
    let scheduler (jobs: Queue<int>) =
      MailboxProcessor<Message>.Start(fun inbox ->
        let rec loop (jobs: Queue<int>) =
          async {
            let! msg = inbox.Receive(timeoutLimit)
            match msg with
            | Ready (id, agent) when jobs.Count <> 0 ->
              let n = jobs.Dequeue()
              agent.Post(Fib n)

              return! loop (jobs)
            | Ready (id, agent) ->

              agent.Post(ShutdownSolver id)
              
              return! loop(jobs)
            | ShutdownScheduler id  ->
              (inbox :> IDisposable).Dispose()
              //()
            | _ -> return! loop (jobs)

          }

        loop (jobs))

  module Summary =

    let doSummary (result: (int * int) list) =
      async {
        printfn "\n============"
        printfn "%A" result
      }

    let summary (nSolvers) (scheduler: MailboxProcessor<Message>) =
        MailboxProcessor<Message>.Start(fun inbox ->
          let rec loop (nSolvers, results) =
            async {
              let! msg = inbox.Receive(timeoutLimit)
              match msg with
              | Answer (x, fibX) ->
                return! loop(nSolvers, (x, fibX) :: results)
               
              | ShutdownSolver id when id <> "scheduler" ->
                return! loop(nSolvers - 1, results)

              | Summary chnl when nSolvers = 0 ->
                chnl.Reply (true, results)
                scheduler.Post(ShutdownScheduler "scheduler")

                (inbox :> IDisposable).Dispose()
              | Summary chnl ->
                chnl.Reply (false, results)
                return! loop(nSolvers, results)
              | _ ->
                return! loop(nSolvers, results)
            }

          loop(nSolvers, [])
        )

  module Solver =
    let rec fib n =
      match n with
      | 0 -> 0
      | 1 -> 1
      | _ -> fib (n - 1) + fib (n - 2)

    let solver (id: string) (scheduler: MailboxProcessor<Message>) (summary: MailboxProcessor<Message>) =
      MailboxProcessor.Start(fun inbox ->
        let rec loop() =
          async {
            scheduler.Post(Ready (id, inbox))

            let! msg = inbox.Receive(timeoutLimit)
            match msg with
            | Fib n ->
              let fibN = fib n
              //printfn $"Solver {id} finished computing fib({n}) = {fibN}, forward result to summary"
              summary.Post(Answer(n, fibN))

              return! loop()
            | ShutdownSolver id->
              
              summary.Post(ShutdownSolver id)
              //printfn $"Solver {id} is shuting down"

              (inbox :> IDisposable).Dispose()
              
            | _ ->
              return! loop()
          }
        loop()
      )





  let demoMeasureSolvingFibWithDifferentSolvers () =

    let duration f =
      let timer = new System.Diagnostics.Stopwatch()
      timer.Start()
      f()
      timer.ElapsedMilliseconds

    let computeFibs nWorkers nJobs fibX =
      let jobs = new Queue<int>(Seq.replicate nJobs fibX)

      let scheduler = Scheduler.scheduler(jobs)
      let summary = Summary.summary nWorkers scheduler

      [1 .. nWorkers]
      |> List.map(fun id -> Solver.solver $"{id}" scheduler summary)
      |> ignore

      async {
        let mutable finished = false
        while not finished do
          let! (allDone, fibs) = summary.PostAndAsyncReply(fun reply -> Summary reply)
          if (allDone) then
            do! Summary.doSummary fibs
          finished <- allDone
      }
      |> Async.RunSynchronously

    let measureNworkers nWorkers nJobs fibX =
        let cost = duration (fun _ -> computeFibs nWorkers nJobs fibX)
        printfn $"{nWorkers} workers tooks: {cost}ms"

    [ 1..10 ]
    |> List.iter (fun n -> measureNworkers n 30 37)




