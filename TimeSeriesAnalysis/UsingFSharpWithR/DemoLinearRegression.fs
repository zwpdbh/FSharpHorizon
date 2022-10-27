module DemoLinearRegression

open RDotNet
open RProvider
open RProvider.Operators

open RProvider.graphics
open RProvider.stats

let linearRegressionDemo () = 
    
    // Random number generator
    let rng = System.Random()
    let rand () = rng.NextDouble()

    // Generate fake X1 and X2 
    let X1s = [ for i in 0 .. 9 -> 10. * rand () ]
    let X2s = [ for i in 0 .. 9 -> 5. * rand () ]

    // Build Ys, following the "true" model
    let Ys = [ for i in 0 .. 9 -> 5. + 3. * X1s.[i] - 2. * X2s.[i] + rand () ]
   
    let dataset = [ 
        "Y" => Ys
        "X1" => X1s
        "X2" => X2s ] |> R.data_frame

    let result = R.lm(formula = "Y~X1+X2", data = dataset)

    let coefficients = result.AsList().["coefficients"].AsNumeric()
    let residuals = result.AsList().["residuals"].AsNumeric()

    let summary = R.summary(result)
    summary.AsList().["r.squared"].AsNumeric() |> ignore

    R.plot result

