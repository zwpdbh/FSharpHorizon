namespace FunctionalPatterns.ReactiveProgramming

module AboutObserable = 
    open System;
    open System.Timers;
    open System.Threading.Tasks;

    let demo01 () =
        let t1 = new Timer(float 500);
        let disposable = 
            t1.Elapsed 
                |> Observable.map(fun _ -> "t1") 
                |> Observable.subscribe(fun s -> printfn "%s" s)
        
        t1.Start()
        Task.Delay(10000).GetAwaiter().GetResult();
        disposable.Dispose ()

    let demo02 () = 
        // Read from: https://medium.com/@dagbrattli/reactivity-in-f-4540377d02fa
        // TBD: Just skim through but get confused, didn't understand it.
        0


/// Play: http://www.fssnip.net/7Z/title/Sliding-window-for-Observable
module SlideWindowForObservable =
    open System

    module SlideWindow =
        /// Returns an observable that yields sliding windows of
        /// containing elements drawn from the input observable.
        /// Each window is returned as a fresh array.
        let windowed (count: int) (source: IObservable<_>) =
            { new IObservable<_> with
                member x.Subscribe(observer) =
                    // Start an agent that remembers partial windows of length
                    // smaller than the count (new agent for every observer)
                    let agent =
                        MailboxProcessor.Start (fun agent ->
                            // The parameter 'lists' contains partial lists and their lengths
                            let rec loop lists =
                                async {
                                    // Receive the next value
                                    let! value = agent.Receive()

                                    // Add new empty list and then the new element to all lists.
                                    // Then split the lists into 'full' that should be sent
                                    // to the observer and 'partial' which need more elements.
                                    let full, partial =
                                        ((0, []) :: lists)
                                        |> List.map (fun (length, l) -> length + 1, value :: l)
                                        |> List.partition (fun (length, l) -> length = count)

                                    // Send all full lists to the observer (as arrays)
                                    for (_, l) in full do
                                        observer.OnNext(l |> Array.ofSeq |> Array.rev)
                                    // Continue looping with incomplete lists
                                    return! loop partial
                                }

                            // Start with an empty list of partial lists
                            loop [])

                    // Send incoming values to the agent
                    source.Subscribe(agent.Post) }


    // Learned from: https://stackoverflow.com/questions/3845110/observable-from-sequence-in-f
    // TBD:: understand https://stackoverflow.com/questions/2649161/need-help-regarding-async-and-fsi
    module MyObservable =
        /// Creates an observable that calls the specified function after someone
        /// subscribes to it (useful for waiting using 'let!' when we need to start
        /// operation after 'let!' attaches handler)
        let guard f (e: IObservable<'Args>) =
            { new IObservable<'Args> with
                member x.Subscribe(observer) =
                    let rm = e.Subscribe(observer) in
                    f ()
                    rm }

        let ofSeq s =
            let evt = new Event<_>()

            evt.Publish
            |> guard (fun o ->
                for n in s do
                    evt.Trigger(n))

        let demoOfSeq () =
            [ 1 .. 10 ] |> ofSeq
            |> Observable.filter (fun n -> n%2 = 0)
            |> Observable.add (printfn "%d")