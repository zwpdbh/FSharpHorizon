
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

        
        Common.readInput @"FromInterview\words.txt" |> printfn "%A"

        0