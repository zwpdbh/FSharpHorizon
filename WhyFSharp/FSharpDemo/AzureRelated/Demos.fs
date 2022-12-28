namespace AzureWorkflow

module DemoAzureSKD = 
    open Azure.ResourceManager
    open Azure.Identity

    let azureClient = new ArmClient(new DefaultAzureCredential(), defaultSubscriptionId = Auth.Setting.deploymentServiceSubScription.Id)

    // Use default Azure Credential
    let demoListResourceGroupUsingAzureSKD () = 
        async {
            let! subscription = azureClient.GetDefaultSubscriptionAsync() |> Async.AwaitTask
            let resourceGroups = subscription.GetResourceGroups()
            for each in resourceGroups do 
                printfn $"{each.Data.Name}"
        } |> Async.RunSynchronously    


module DemoAuthAgent = 
    open Auth.AuthAgent
    open Auth.Setting

    let demoGetAuthTokenUsingServicePrincipal () = 
        let authTokenAgent = new AuthTokenAgent(zwpdbhSP, azureScope)
        try 
            authTokenAgent.RequestNewToken() |> Async.RunSynchronously |> printfn "RequestNewToken: %A" |> ignore 
            printfn "==="
            authTokenAgent.GetAccessToken() |> Async.RunSynchronously |> printfn "GetAccessToken: %A" |> ignore 
            printfn "==="
            authTokenAgent.RequestNewToken() |> Async.RunSynchronously |> printfn "RequestNewToken: %A" |> ignore 
            printfn "==="
            authTokenAgent.GetAccessToken() |> Async.RunSynchronously |> printfn "GetAccessToken: %A" |> ignore 
            printfn "==="
            authTokenAgent.GetAccessToken() |> Async.RunSynchronously |> printfn "GetAccessToken: %A" |> ignore 
        with 
            | ex -> printfn "%A" ex 
        printfn "==="
        printfn "Done"
