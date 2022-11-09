module DemoBasics


open RProvider
open RDotNet
open System
open RProvider.faraway
open RProvider.graphics
open RProvider.grDevices
open RProvider.Operators

//R commands like: 
//data(coagulation, package='faraway')
//summary(coagulation), the result is like:
 //     coag       diet 
 //Min.   :56.00   A:4  
 //1st Qu.:61.75   B:6  
 //Median :63.50   C:6  
 //Mean   :64.00   D:8  
 //3rd Qu.:67.00        
 //Max.   :71.00        
 // See: https://fslab.org/RProvider/passing-data.html
let summaryDemo() = 
    let dataset = faraway.R.coagulation
    let summary = R.summary(dataset).AsList()
    for each in summary do 
        for x in each.AsList() do 
            printfn "%A" (x.AsNumeric())
    //printfn "%A" (summary.["coag"].AsNumeric())

// From R:
//> data.1 = c(35,8,10,23,42)
//> data.1
//[1] 35  8 10 23 42
//> data.1
//[1] 35  8 10 23 42
//> summary(data.1)
//   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
//    8.0    10.0    23.0    23.6    35.0    42.0 
//> mean(data.1)
//[1] 23.6
//> sum(data.1)/5
//[1] 23.6
//> sd(data.1)
//[1] 14.97665
let demo01 () = 
    let data = [35; 8; 10; 23; 42]
    // remember the corresponding R type to .NET type using "AsXXX" is always result in sequence
    // See: https://fslab.org/RProvider/passing-data.html to the the convert R data into the .NEt type
    let mean = R.mean(data).AsNumeric().[0]
    printfn "%A" mean  // -> 23.6

    let summary = R.summary(data).AsNumeric()
    printfn "%A" summary // -> seq [8.0; 10.0; 23.0; 23.6; ...]

open RProvider.stats



// In R it will be 
// small.size.dataset=c(91,49,76,112,97,42,70, 100, 8, 112, 95, 90, 78, 62, 56, 94, 65, 58, 109, 70, 109, 91, 71, 76, 68, 62, 134, 57, 83, 66)
//hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green', breaks=10)
//lines(density(small.size.dataset), col='red', lwd=5)
let demo02 () = 
    let data = [91; 49; 76; 112; 97; 42; 70; 100; 8; 112; 95; 90; 78; 62; 56; 94; 65; 58; 109; 70; 109; 91; 71; 76; 68; 62; 134; 57; 83; 66]
    // The way to specify multiple argument see: https://fslab.org/RProvider/quickstart-charts.html#Named-Parameters
    R.hist([
        "x" => data
        "breaks" => 10
        "col" => "green"
        "main" => "Histogram of my data"
        "freq" => false 
        "xlab" => "My data points"
    ]) |> ignore
    R.lines([
        "x" => R.density(data)
        "col" => "red"
        "lwd" => 5
    ])


let demo03 () = 
    R.set_seed(2016) |> ignore
    let data01 = 
        R.rnorm([
            "n" => 50
            "mean" => 78
            "sd" => 10
        ])
        |> R.round
    let data02 = 
        R.rnorm([
            "n" => 50
            "mean" => 70
            "sd" => 12
        ])      
        |> R.round

    R.plot([
        "x" => data01 
        "y" => data02 
        "main" => "Test scores for two exams (50 students)"
        "xlab" => "data01"
        "ylab" => "data02"
        "col" => "blue"
    ])