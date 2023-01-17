namespace AzureAPI

module Snapshot =
    
    let listAllSnapshots (subscriptionId: string ) = 
        // GET https://management.azure.com/subscriptions/{subscriptionId}/providers/Microsoft.Compute/snapshots?api-version=2021-12-01
        let endpoint = $"https://management.azure.com/subscriptions/{subscriptionId}/providers/Microsoft.Compute/snapshots"
        let queryParameters: Map<string, string> = 
            Map.empty
                .Add ("api-version", "2021-12-01")
        async {
            let! responseStr = HttpClient.sendRequestAsync HttpClient.Get endpoint None (Some queryParameters)
            return responseStr
        }


