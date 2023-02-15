﻿namespace FunctionalPatterns

module Main = 
    [<EntryPoint>]
    let main argv = 
        // RailwayProgramming.FizzBuzzUsingPipelineV1.demo ()
        // RailwayProgramming.FizzBuzzUsingPipelineV2.demo ()
        //ApplicativeParsers.ParserCombinators.demoLifting ()
        //Playground.ParserFoundations.testNegativeInteger() 

        ReactiveProgramming.EventsToStreams.SimpleEventSystem.demo ()

        0