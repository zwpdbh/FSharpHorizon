
namespace Others

module OthersMain =
    open Expecto

    [<EntryPoint>]
    let main argv =
        //Tests.runTestsInAssembly defaultConfig argv

        //Others.WorkingWithData.PostgreSql.demoInsertOneRow () 
        //Others.WorkingWithData.PostgreSql.insertStudents ()
        //Others.WorkingWithData.PostgreSql.NpgsqlFSharp.initDB ()

        //Others.WorkingWithData.PostgreSql.SqlProvider.demos ()

        
        // Common.readInput @"FromInterview\words.txt" |> printfn "%A"
        //Common.filePathInProject @"ComputationExpression\PNG_01.png" |> printfn "%A"

        //Snippets.ProcessText.Frequence.demoMostFrequence () |> printfn "%A"
        //Snippets.SlideWindow.SubStringProblem.demo () |> printfn "%A"
        //Snippets.SlideWindow.SubStringProblemActivePattern.demo () |> printfn "%A"
        //FromInterview.Polymer.Problem01.demo () |> printfn "%A"


        //FromInterview.Ahrefs.Problem01.demo () |> printfn "%A"
        //Others.Snippets.SlideWindow.SubString.demo () 

        //Others.Snippets.DecodeJson.DecodeMapResponse.demo() |> printfn "%A"
        Others.Snippets.DecodeJson.DecodeWorkflowsResponseV2.demo () |> printfn "%A"
        0