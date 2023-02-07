namespace AksManagement

module Snapshot = 
    open RestAPI  
    
    let listAllSnapshots (subscriptionId: string ) (accessToken: string) = 

        // GET https://management.azure.com/subscriptions/{subscriptionId}/providers/Microsoft.Compute/snapshots?api-version=2021-12-01
        let endpoint = $"https://management.azure.com/subscriptions/{subscriptionId}/providers/Microsoft.Compute/snapshots"
        let queryParameters: Map<string, string> = 
            Map.empty
                .Add ("api-version", "2021-12-01")

        let headerParameters: Map<string, string> = 
            Map.empty
                .Add("Authorization", $"Bearer {accessToken}")

        async {
            let! responseStr = HttpClient.getRequestAsync endpoint (Some headerParameters) (Some queryParameters)
            return responseStr
        }

    let demoListSnapshot () = 
        async {
            let! accessTokenResponse = Auth.AzureAuthService.getAccessToken()
            match accessTokenResponse with 
            | Result.Ok accessToken -> 
                return! listAllSnapshots "33922553-c28a-4d50-ac93-a5c682692168" accessToken    
            | Result.Error err -> 
                printfn "No access token"
                //failwith err 
                return err 
        } 
        |> Async.RunSynchronously
        |> printfn "%A"

    /// TBD: get a xscnworkflow, find all its related snapshot!


module XscnWorkflowConsole = 
    open RestAPI

    let listWorkflowInstances recordsLimitToFetch accessToken =         
        let endpoint = $"https://xscnworkflowconsole.eastus.cloudapp.azure.com/api/Workflow"
        let headerParameters: Map<string, string> = 
            Map.empty
                .Add("accept", "text/plain")
                .Add("Authorization", $"Bearer {accessToken}")
        let queryParameters: Map<string, string> = 
            Map.empty
                .Add ("count", $"{recordsLimitToFetch}")
        async {
            let! resultStr = HttpClient.getRequestAsync endpoint (Some headerParameters) (Some queryParameters)
            return resultStr
        }  

    let demoListWorkflowInstances () = 
        async {
            let! accessTokenResponse = Auth.XscnWorkflowConsoleAuthService.getAccessToken()
            match accessTokenResponse with 
            | Result.Ok accessToken -> 
                return! listWorkflowInstances 10 accessToken
            | Result.Error err -> 
                return err 
        } 
        |> Async.RunSynchronously
        |> printfn "%A"