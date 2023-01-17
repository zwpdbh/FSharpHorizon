namespace AzureAPI

module Snapshot =
    
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


