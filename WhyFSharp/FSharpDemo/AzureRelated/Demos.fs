namespace AzureWorkflow


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
    // Shows the example using device code grant to request access_token via HTTP request concurrently
    open Auth.AuthAgent
    open Auth.Setting

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


    let demoGetAuthTokenUsingServicePrincipalv2 () =
        // The point is we could chain more async steps to further delay avoid randevu point
        let asyncMap f computation =
            async {
                let! x = computation
                return f x
            }

        let authTokenAgent = new AuthTokenAgent(zwpdbhSP, azureScope)
        [1..5]
        |> List.map (fun _ -> 
            authTokenAgent.GetAccessToken()
            )
        |> Async.Parallel   
        |> asyncMap (Array.choose (fun response -> 
            match response with 
            | Result.Ok authTokenResponse -> Some authTokenResponse
            | _ -> None
            )
        )
        |> Async.RunSynchronously 
        |> Array.iter (fun each -> 
            printfn "RequestNewToken: %A" each|> ignore 
            printfn "===" 
        )
        printfn "Done"


module DemoAzureResourceManager = 
    // https://learn.microsoft.com/en-us/dotnet/api/overview/azure/resourcemanager-readme?view=azure-dotnet
    // Microsoft Azure Resource Manager is the deployment and management service for Azure. 
    // It provides a management layer that enables you to create, update, and delete resources in your Azure account.
    open Azure.ResourceManager
    open Azure.Identity

    // Especially from: https://learn.microsoft.com/en-us/dotnet/api/azure.resourcemanager.armclient?view=azure-dotnet
    let azureClient = new ArmClient(new DefaultAzureCredential(), defaultSubscriptionId = Auth.Setting.deploymentServiceSubScription.Id)

    // Use default Azure Credential
    let demoListResourceGroupUsingDefaultAzureCredential () = 
        async {
            let! subscription = azureClient.GetDefaultSubscriptionAsync() |> Async.AwaitTask
            let resourceGroups = subscription.GetResourceGroups()
            for each in resourceGroups do 
                printfn $"{each.Data.Name}"
        } |> Async.RunSynchronously    


module DemoAzureStorageBlob = 
    // See: https://learn.microsoft.com/en-us/dotnet/api/overview/azure/storage.blobs-readme?view=azure-dotnet
    open Azure.Storage.Blobs
    //The storage account used via BlobServiceClient
    //A container in the storage account used via BlobContainerClient
    //A blob in a container used via BlobClient

    open System
    open Azure.Identity

    type CredentialType = 
        | DefaultAzureCredential 
        | ServicePrincipal 

    let getStorageAccount (storageAccountName: string) (credentialType: CredentialType) = 
        // https://learn.microsoft.com/en-us/dotnet/api/overview/azure/storage.blobs-readme?view=azure-dotnet
        let accountUri =  new Uri($"https://{storageAccountName}.blob.core.windows.net/")

        match credentialType with 
        | DefaultAzureCredential -> 
            new BlobServiceClient(accountUri, new DefaultAzureCredential())
        | ServicePrincipal -> 
            let clientCertificateCredential = new ClientCertificateCredential(
                Auth.Setting.zwpdbhSP.tenantId, 
                Auth.Setting.zwpdbhSP.clientId, 
                Auth.KeyVault.getLocalCertificateBySubject "zwpdbhREST"
            )
            new BlobServiceClient(accountUri, clientCertificateCredential)

    let demoListContainers () = 
        let storageAccountClient = getStorageAccount "zwpdbh" ServicePrincipal
        storageAccountClient.GetBlobContainers()
        |> Seq.iter (fun eachContainer -> 
            printfn $"{eachContainer.Name}"
        )
