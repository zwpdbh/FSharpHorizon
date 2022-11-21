﻿//// This works as well
//open System
//open System.Drawing
//open System.Windows.Forms

//open GUI

//Application.EnableVisualStyles()
//Application.SetCompatibleTextRenderingDefault(false)
//let view = new SampleForm()
//Application.Run(view)



// This is the example from book, maybe a little old fashion
namespace Program

open System
open System.Drawing
open System.Windows.Forms

open GUI

module Main = 
    [<STAThreadAttribute>]
    do 
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault(false)
        
        // example 01: click button
        // let view = new SampleForm()

        // example 02: display text
        //let view = new TextForm() 

        let view = new TableForm()

        Application.Run(view)

