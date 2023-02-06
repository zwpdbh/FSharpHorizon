namespace AKSManagement

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

    let demoListSnapshot () : string = 
        async {
            let! accessTokenResponse = AzureAuth.AuthService.getAccessToken()
            match accessTokenResponse with 
            | Result.Ok accessToken -> 
                return! listAllSnapshots "33922553-c28a-4d50-ac93-a5c682692168" accessToken    
            | Result.Error err -> 
                printfn "No access token"
                //failwith err 
                return err 
        } |> Async.RunSynchronously

    /// TBD: get a xscnworkflow, find all its related snapshot!