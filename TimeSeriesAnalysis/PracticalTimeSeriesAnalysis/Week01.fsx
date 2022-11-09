/// Run this into FSI REPL to make R related available in REPL
#r "nuget:RProvider" 
#r "nuget:Deedle"
#r "nuget:Plotly.NET"

open Deedle
open Plotly.NET

open RProvider
open RProvider.faraway
open RProvider.datasets


let demoDeedle () = 
    // Demo from http://bluemountaincapital.github.io/Deedle/rinterop.html
    // However, instead of using FSharp.Charting. I am using Plotly.NET which is more robust.
    let mtcars : Frame<string, string> = R.mtcars.GetValue()
    mtcars
    |> Frame.groupRowsByInt "gear"
    |> Frame.getCol "mpg"
    |> Stats.levelMean fst
    |> Series.observations
    |> Chart.Column
    |> Chart.show 