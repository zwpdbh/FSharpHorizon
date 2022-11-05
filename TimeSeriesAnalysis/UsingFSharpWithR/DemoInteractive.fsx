
#r "nuget:RProvider"


open RDotNet
open RProvider
open RProvider.Operators

open RProvider.graphics
open RProvider.stats


fsi.AddPrinter FSIPrinters.rValue

let rng = System.Random()
let rand () = rng.NextDouble()

let X1s = [ for i in 0 .. 9 -> 10. * rand () ]

let X2s = [ for i in 0 .. 9 -> 5. * rand () ]
let Ys = [ for i in 0 .. 9 -> 5. + 3. * X1s.[i] - 2. * X2s.[i] + rand () ]

let dataset = [ 
        "Y" => Ys
        "X1" => X1s
        "X2" => X2s ] |> R.data_frame

let result = R.lm(formula = "Y~X1+X2", data = dataset)

R.plot result