//See: https://learn.microsoft.com/en-us/dotnet/fsharp/tools/fsharp-interactive/
//F# Interactive supports referencing NuGet packages with the #r "nuget:" syntax and an optional version:


#r "nuget: Plotly.NET,  2.0.0-preview.8"
#r "nuget: Plotly.NET.Interactive,  2.0.0-preview.8"

open Plotly.NET 

open Plotly.NET.TraceObjects
let N = 100
let rnd = System.Random()
let x = Array.init N (fun _ -> rnd.NextDouble())
let y = Array.init N (fun _ -> rnd.NextDouble())

let marker = Marker.init(Size= 30,Colorscale=StyleParam.Colorscale.Viridis);
marker?color <- x
let scatter =
  Chart.Scatter(x,y,StyleParam.Mode.Markers,Opacity=0.6)
  |> Chart.withMarker(marker)
  // |> Chart.withSize(800.,500.)

scatter

// How to run script?
// (optional) check: https://github.com/nblockchain/fsx
// TBD: https://medium.com/@KarandikarMihir/fsharp-learning-to-use-fake-and-paket-f8f68a85d552

// Check env directory in FSI:
// System.Environment.CurrentDirectory;;