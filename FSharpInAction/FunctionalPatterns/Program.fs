﻿namespace FunctionalPatterns

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
        ReactiveProgramming.SlideWindowForObservable.MyObservable.demoOfSeq ()
        0