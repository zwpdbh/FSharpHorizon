
namespace Others

module OthersMain =
    open Expecto
    open System.Text.RegularExpressions
    [<EntryPoint>]
    let main argv =

        FromInterview.Tmp.Problem01.demo () |> printfn "%A"

        0