namespace XscnXscnario

/// References:
/// https://xscenariodeployments.cloudapp.net/swagger/ui/index#!/VirtualMachineDeployments/VirtualMachineDeployments_PutDeployment
/// ArmTemplates stored at: https://msazure.visualstudio.com/One/_git/Storage-XPerfInfra?path=/etc/ArmTemplates

open RestAPI

module XscnScenarioDemos = 
    let xscnUri = "https://xscenariodeployments.cloudapp.net"

    let listVirtualMachineDeployments (accessToken: string) =
        let endpoint = xscnUri + "/api/virtualmachinedeployments"
        let headerParameters: Map<string, string> = 
            Map.empty
                .Add("Authorization", $"Bearer {accessToken}")
        async {
            let! responseStr = HttpClient.getRequestAsync endpoint (Some headerParameters) None 
            return responseStr
        } 


    let demoListVirtualMachineDeployments () = 
        async {
            let! accessTokenResponse = Auth.XscnScenarioAuthService.getAccessToken()
            match accessTokenResponse with 
            | Result.Ok accessToken -> 
                return! listVirtualMachineDeployments accessToken    
            | Result.Error err -> 
                printfn "No access token"
                return err 
        } 
        |> Async.RunSynchronously
        |> printfn "%A"