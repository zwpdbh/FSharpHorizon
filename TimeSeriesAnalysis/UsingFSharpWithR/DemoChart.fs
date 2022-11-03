module DemoChart

/// See requirement: https://github.com/fslaborg/RProvider#requirements
/// Note: on Windows, there is currently a bug in R preventing us from supporting R versions greater than 4.0.2.
open RProvider
//#r "nuget:RProvider"

open RProvider.graphics
open RProvider.grDevices // Required package to save charts
open RProvider.datasets
open RProvider.Helpers
open RProvider.faraway // Once fresh installed a package, it seems we need to restart VS to fresh the env to see the packages
open RDotNet
open System

let chartDemo () = 
    // See: http://bluemountaincapital.github.io/FSharpRProvider/Charts-QuickStart.html
    // Create path to an image testimage.png on the Desktop
    let desktop = Environment.GetFolderPath(Environment.SpecialFolder.Desktop)  
    let path = desktop + @"\testimage.png"

    let widgets = [ 3; 8; 12; 15; 19; 18; 18; 20; ]

    // Open the device and create the file as a png.
    // R.bmp, R.jpeg, R.pdf, ... will generate other formats.
    R.png(filename=path, height=200, width=300, bg="white") |> ignore
    // Create the chart into the file
    R.barplot(widgets) |> ignore
    // Close the device once the chart is complete
    R.dev_off ()

//R commands like: 
//data(coagulation, package='faraway')
//plot(coag~diet, data=coagulation)
let plotDemo () =
    let dataset = faraway.R.coagulation.AsList()

    // R.plot(coag, diet) |> ignore
    for x in dataset do 
        for i in x.AsNumeric() do 
            printfn "%A" i // -> 62.0; 60.0; 63.0; 59.0; ...
        
    let coag = dataset.[0]
    printfn "%A" (coag.AsNumeric()) // -> seq [62.0; 60.0; 63.0; 59.0; ...]
    let diet = dataset.[1]
    printfn "%A" (diet.AsNumeric()) // -> seq [1.0; 1.0; 1.0; 1.0; ...]
    //printfn "%A" (seq {for x in dataset -> x.AsNumeric})
