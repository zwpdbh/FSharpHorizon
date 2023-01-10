namespace AzureWorkflow

module Extention = 
    type Async with 
        static member Map f computation =
            async {
                let! x = computation
                return f x
            }
        
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
    open Extention
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
        let authTokenAgent = new AuthTokenAgent(zwpdbhSP, azureScope)
        [1..5]
        |> List.map (fun _ -> 
            authTokenAgent.GetAccessToken()
            )
        |> Async.Parallel   
        // The point is we could chain more async steps to further delay avoid randevu point
        |> Async.Map (Array.choose (fun response -> 
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
    open System.IO
    open Azure.Identity
    open Extention

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


    let demoTestMemoryStream () = 
        use stream = new MemoryStream()
        let bytes = System.Text.Encoding.ASCII.GetBytes("This is a sample text.")

        stream.Write(bytes, 0, bytes.Length)              
        stream.ToArray() |> System.Text.Encoding.UTF8.GetString |> printfn "%A"
        // If we want to convert from stream to Base64 string 
        // stream.ToArray() |> Convert.ToBase64String
   
    let updateBlob (containerClient: BlobContainerClient) (localFilePath: string) = 
        async {
            let fileName = Path.GetFileName(localFilePath)
            let blobClient = containerClient.GetBlobClient(fileName)    

            use stream = new MemoryStream()
            let bytes = System.Text.Encoding.ASCII.GetBytes("This is a sample text.")
            stream.Write(bytes, 0, bytes.Length)

            return! blobClient.UploadAsync(stream) |> Async.AwaitTask
        }


    //let listBlob (blobContainer: BlobContainerClient) = 
    //    async {
    //        // IAsyncEnumerable<T> to Task<IEnumerable<T>>
    //        let resultSegment = blobContainer.GetBlobsAsync().AsPages(null, 10)
    //        let something = resultSegment |> Seq.cast<Azure.Page<Models.BlobItem>>
                
    //        //let blobs = 
    //        //    seq {
    //        //        for (each: Azure.Page<BlobItem>) in resultSegment.
    //        //    }
    //    }

    let demo () = 
        let storageAccountClient = getStorageAccount "zwpdbh" ServicePrincipal
        let blobFileNames =
            seq {
                for each in [1..10] do 
                    yield $"test{each}"
            }
    
        let blobContainer = storageAccountClient.GetBlobContainerClient("testblob")

        blobFileNames 
        |> Seq.map (updateBlob blobContainer)
        |> Async.Parallel
        |> Async.Map (Seq.choose (fun response -> 
            match response.HasValue with 
            | true -> Some response.Value
            | false -> None 
        ))


module DemoAzureIncrementalCopy = 
    open Azure.Storage.Blobs
    open Azure.Storage.Blobs.Specialized
    open System
    open Azure.Identity

    let getStorageAccount (storageAccountName: string) = 
        // https://learn.microsoft.com/en-us/dotnet/api/overview/azure/storage.blobs-readme?view=azure-dotnet
        let accountUri =  new Uri($"https://{storageAccountName}.blob.core.windows.net/")

        let clientCertificateCredential = new ClientCertificateCredential(
                Auth.Setting.zwpdbhSP.tenantId, 
                Auth.Setting.zwpdbhSP.clientId, 
                Auth.KeyVault.getLocalCertificateBySubject "zwpdbhREST"
            )
        new BlobServiceClient(accountUri, clientCertificateCredential)
       
    
    let demo () = 
        // See: https://learn.microsoft.com/en-us/azure/storage/blobs/storage-blob-pageblob-overview?tabs=dotnet#sample-use-cases
        let blobServiceClient = getStorageAccount "zwpdbh"
        let blobContainerClient = blobServiceClient.GetBlobContainerClient("testincremental")

        // That size must be a multiple of 512 bytes.
        let oneGigaByteAsBytes = int64 (1024 * 1024 * 1024)

        let pageBlobClient = blobContainerClient.GetPageBlobClient("test.vhd")
        let response = pageBlobClient.Create(int64 16 * oneGigaByteAsBytes)
        printfn $"{response}"