module Week01

open RProvider
open RDotNet
open System
open RProvider.faraway
open RProvider.graphics
open RProvider.grDevices
open RProvider.Operators

open RProvider.stats


// For example from PDF file https://www.coursera.org/learn/practical-time-series-analysis/supplement/laBLx/basic-statistics-review-with-linear-regression-and-hypothesis-testing
// 𝑌𝑖 = 𝑙𝑖𝑛𝑒𝑎𝑟 𝑚𝑜𝑑𝑒𝑙 𝑝𝑙𝑢𝑠 𝑛𝑜𝑖𝑠𝑒 = ( 𝛽0 + 𝛽1𝑥𝑖) + 𝜖

let co2Data = datasets.R.co2
let co2Times = R.time(co2Data)

let dataset = [
    "Y" => co2Data 
    "X" => co2Times] |> R.data_frame 
let lmModel = R.lm(formula = "Y~X", data = dataset)

let demo01 () = 
    
    // Plot the linear regression
    R.plot([
        "x" => co2Data 
        "main" => "Atmospheric CO2 Concentration with Fitted Line"
    ]) |> ignore
    R.abline(lmModel) |> ignore


let demo02 () = 
    //  zoom in on the residuals
    let co2Residuals = R.resid(lmModel)
    R.plot([
        "y" => co2Residuals
        "x" => co2Times 
        "xlim" => R.c(1960, 1963)
        "main" => "Zoomed in Residuals on Time"
    ])
    // See https://fslab.org/RProvider/quickstart-statistics.html about how they handle result


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
    // Somehow to make it works as an application. I also need to install Deedle.RPlugin which is not needed in FSI...