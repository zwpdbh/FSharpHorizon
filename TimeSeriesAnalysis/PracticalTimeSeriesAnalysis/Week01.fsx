#r "nuget:RProvider" // run this into FSI REPL to make R related available in REPL

open RDotNet
open RProvider
open RProvider.Operators

open RProvider.graphics
open RProvider.stats
open RProvider.faraway


// For example from PDF file https://www.coursera.org/learn/practical-time-series-analysis/supplement/laBLx/basic-statistics-review-with-linear-regression-and-hypothesis-testing
// 𝑌𝑖 = 𝑙𝑖𝑛𝑒𝑎𝑟 𝑚𝑜𝑑𝑒𝑙 𝑝𝑙𝑢𝑠 𝑛𝑜𝑖𝑠𝑒 = ( 𝛽0 + 𝛽1𝑥𝑖) + 𝜖
let co2Data = datasets.R.co2
let co2Times = R.time(co2Data)

let dataset = [
    "Y" => co2Data 
    "X" => co2Times] |> R.data_frame 

let lmModel = R.lm(formula = "Y~X", data = dataset)

// Plot the linear regression
R.plot([
    "x" => co2Data 
    "main" => "Atmospheric CO2 Concentration with Fitted Line"
]) |> ignore
R.abline(lmModel) |> ignore