namespace FunctionalPatterns

module Main = 
    [<EntryPoint>]
    let main argv = 
        // RailwayProgramming.FizzBuzzUsingPipelineV1.demo ()
        // RailwayProgramming.FizzBuzzUsingPipelineV2.demo ()
        //ApplicativeParsers.ParserCombinators.demoLifting ()
        //Playground.ParserFoundations.testNegativeInteger() 

        // ReactiveProgramming.EventsToStreams.SimpleEventSystem.demoRegisterHandler ()
        //ReactiveProgramming.EventsToStreams.MergingMultipleEvents.demoImperativeOne ()
        //ReactiveProgramming.EventsToStreams.MergingMultipleEvents.demoFunctionalOne ()
        //ReactiveProgramming.SlideWindowForObservable.MyObservable.demoOfSeq ()


        //ReactiveProgramming.Asynchronous.CPS.demoSyncStyle () |> printfn "%A"
        //ReactiveProgramming.Asynchronous.CPS.demoCPSStyle () |> printfn "%A"
        //ReactiveProgramming.Asynchronous.CPS.demoCPSProblem () 

        //ReactiveProgramming.Asynchronous.MyTest.demoOneAfterAnother ()
        //ReactiveProgramming.Asynchronous.MyTest.demoSequenceAsync ()
        //ReactiveProgramming.Asynchronous.MyTest.demoParallelAsync ()

        ReactiveProgramming.Asynchronous.FunAndProfit.demoCancellingWorkflow () 
        0