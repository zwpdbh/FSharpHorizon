namespace AzureWorkflow

module DemoAzureSKD = 
    open Azure.ResourceManager
    open Azure.Identity

    let azureClient = new ArmClient(new DefaultAzureCredential(), defaultSubscriptionId = Auth.Setting.deploymentServiceSubScription.Id)

    // Use default Azure Credential
    let demoListResourceGroupUsingDefaultAzureCredential () = 
        async {
            let! subscription = azureClient.GetDefaultSubscriptionAsync() |> Async.AwaitTask
            let resourceGroups = subscription.GetResourceGroups()
            for each in resourceGroups do 
                printfn $"{each.Data.Name}"
        } |> Async.RunSynchronously    


module DemoAzureKeyVault = 
    open Auth.KeyVault

    let demoGetCertificateByThumbprint () = 
        getLocalCertificateByThumbprint "fbd57d0fc8e7a8fd3154c746b0e1f6e40a5a1029"
        |> printfn "certificate: %A"

    let demoGetCertificateBySubject () = 
        getLocalCertificateBySubject "zwpdbhREST"
        |> printfn "certificate: %A"

    let demoGetSecretValue () = 
        getSecretValue "zwpdbhSPSecret"
        |> Async.RunSynchronously
        |> printfn "secret value: %A" 

module DemoAuthAgent = 
    open Auth.AuthAgent
    open Auth.Setting

    let asyncMap f computation =
        async {
            let! x = computation
            return f x
        }

    let demoGetAuthTokenUsingServicePrincipal () = 
        let authTokenAgent = new AuthTokenAgent(zwpdbhSP, azureScope)
        [1..5]
        |> List.map (fun _ -> 
            authTokenAgent.GetAccessToken()
            )
        |> Async.Parallel 
        |> Async.RunSynchronously 
        |> Array.choose (fun eachResonse -> 
            match eachResonse with 
            | Result.Ok authTokenResponse -> Some authTokenResponse
            | _ -> None
        )
        |> Array.iter (fun each -> 
            printfn "RequestNewToken: %A" each|> ignore 
            printfn "===" 
        )
        printfn "Done"
