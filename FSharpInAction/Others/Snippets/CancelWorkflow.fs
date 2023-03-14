﻿namespace Others.Snippets

// TBD: understand "Asynchronous cancellation of a workflow"

/// The snippet implements Async.StartCancellable method that can be used to start a given workflow and then cancel it.
/// The cancellation of the workflow is done asynchronously, which means that the caller will wait until the workflow is actually cancelled.
/// From: http://www.fssnip.net/d4/title/Asynchronous-cancellation-of-a-workflow
module CancelWorkflow =
    open System

    /// Helper that can be used for writing CPS-style code that resumes
    /// on the same thread where the operation was started.
    let internal synchronize f =
        let ctx = System.Threading.SynchronizationContext.Current

        f (fun g ->
            let nctx = System.Threading.SynchronizationContext.Current

            if ctx <> null && ctx <> nctx then
                ctx.Post((fun _ -> g ()), null)
            else
                g ())

    type Microsoft.FSharp.Control.Async with
        /// Behaves like AwaitObservable, but calls the specified guarding function
        /// after a subscriber is registered with the observable.
        static member GuardedAwaitObservable (ev1: IObservable<'T1>) guardFunction =
            synchronize (fun f ->
                Async.FromContinuations(
                    (fun (cont, econt, ccont) ->
                        let rec finish cont value =
                            remover.Dispose()
                            f (fun () -> cont value)

                        and remover: IDisposable =
                            ev1.Subscribe(
                                { new IObserver<_> with
                                    member x.OnNext(v) = finish cont v
                                    member x.OnError(e) = finish econt e

                                    member x.OnCompleted() =
                                        let msg =
                                            "Cancelling the workflow, because the "
                                            + "Observable awaited using AwaitObservable has completed."

                                        finish ccont (new System.OperationCanceledException(msg)) }
                            )

                        guardFunction ())
                ))

    module Async =
        open System.Threading

        /// Returns an asynchronous workflow 'Async<Async<unit>>'. When called
        /// using 'let!', it starts the workflow provided as an argument and returns
        /// a token that can be used to cancel the started work - this is an
        /// (asynchronously) blocking operation that waits until the workflow
        /// is actually cancelled
        let StartCancellable work =
            async {
                let cts = new CancellationTokenSource()
                // Creates an event used for notification
                let evt = new Event<_>()
                // Wrap the workflow with TryCancelled and notify when cancelled
                Async.Start(Async.TryCancelled(work, ignore >> evt.Trigger), cts.Token)
                // Return a workflow that waits for 'evt' and triggers 'Cancel'
                // after it attaches the event handler (to avoid missing event occurrence)
                let waitForCancel = Async.GuardedAwaitObservable evt.Publish cts.Cancel
                return async.TryFinally(waitForCancel, cts.Dispose)
            }


    let demo () =
        /// Sample workflow that repeatedly starts and stops long running operation
        let loop =
            async {
                for i in 0..9999 do
                    printfn "Starting: %d" i
                    do! Async.Sleep(1000)
                    printfn "Done: %d" i
            }

        // Start the 'loop' workflow, wait for 5.5 seconds and then
        // cancel it and wait until it finishes current operation
        async {
            let! cancelToken = Async.StartCancellable(loop)
            printfn "started"
            do! Async.Sleep(5500)
            printfn "cancelling"
            do! cancelToken
            printfn "done"
        }
        |> Async.Start
