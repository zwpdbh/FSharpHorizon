
namespace Others.WorkingWithData 

module QueryExpressions = 
    // From https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/query-expressions
    // Instead of using SQLClient, we use SQLProvider
    // SQLProvider can cover more databases than SQLClient(only works with MSSQL)
    open FSharp.Data.Sql

    let demo () =
        0