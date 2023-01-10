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
    open Azure.Storage.Blobs.Models
    //The storage account used via BlobServiceClient
    //A container in the storage account used via BlobContainerClient
    //A blob in a container used via BlobClient

    open System
    open System.IO
    open Azure.Identity
    open Extention
    open FSharp.Control

    // During test, I found I could list the containers from storage account: zwpdbh. 
    // But I couldn't upload blob into any containers in that storage account.
    // That's because my SP has Contributor role inherited from Subscription.
    // However, to read, write and delete access to storage container and blob. I need to assign: Storage Blob Data Contributor to my SP!


    type CredentialType = 
        | DefaultAzureCredential 
        | ServicePrincipal 

    let getBlobServiceClient (storageAccountName: string) (credentialType: CredentialType) = 
        // https://learn.microsoft.com/en-us/dotnet/api/overview/azure/storage.blobs-readme?view=azure-dotnet
        let accountUri =  new Uri($"https://{storageAccountName}.blob.core.windows.net/")

        match credentialType with 
        | DefaultAzureCredential -> 
            new BlobServiceClient(accountUri, new DefaultAzureCredential())
        | ServicePrincipal -> 
            // The assigned IAM scope is:
            // /subscriptions/<subscription_id>/resourceGroups/zwpdbh/providers/Microsoft.Storage/storageAccounts/zwpdbh
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
        
    let blobServiceClient = getBlobServiceClient "zwpdbh" CredentialType.ServicePrincipal

    let getBlobContainer containerName = 
        let blobContainer = blobServiceClient.GetBlobContainerClient containerName

        // This doesn't work, will has resource not found problem...
        //blobContainer.CreateIfNotExists() |> ignore 
        //blobContainer.SetAccessPolicy(PublicAccessType.Blob) |> ignore

        blobContainer.CreateIfNotExists(PublicAccessType.Blob) |> ignore

        printfn $"get blob container, its uri: {blobContainer.Uri}"
        blobContainer

    let demoListBlobContainers () = 
        blobServiceClient.GetBlobContainers()
        // Loop over Azure.Pageable 
        |> Seq.iter (fun eachContainer -> 
            printfn $"{eachContainer.Name}"
        )

    let demoFromMicrosft () = 
        let prefix = System.DateTime.Now.ToString "yyyyMMdd-HHmmss"
        // https://learn.microsoft.com/en-us/dotnet/fsharp/using-fsharp-on-azure/blob-storage
        // Create a dummy file to upload
        let localFile = "./myfile.txt"
        File.WriteAllText(localFile, "some data")

        let container = getBlobContainer ("mycontainer" + prefix)

        // Retrieve reference to a blob named "myblob.txt".
        let blockBlob = container.GetBlobClient("myblob.txt")

        // Create or overwrite the "myblob.txt" blob with contents from the local file.
        use fileStream = new FileStream(localFile, FileMode.Open, FileAccess.Read, FileShare.Read)
        do blockBlob.Upload(fileStream) |> ignore

        // List the blobs in a container
        //for item in container.GetBlobsByHierarchy() do
        //    printfn $"Blob name: {item.Blob.Name}"

        // OR 
        // Call GetBlobsByHierarchy to return an async collection (it is not IAsyncEnumerable, so it is async ?)
        // of blobs in this container. AsPages() method enumerate the values 
        //a Page<T> at a time. This may make multiple service requests.
        for page in container.GetBlobsByHierarchy().AsPages() do
            for blobHierarchyItem in page.Values do 
                printf $"The BlobItem is : {blobHierarchyItem.Blob.Name} "

        // Download blobs
        let blobToDownload = container.GetBlobClient("myblob.txt")

        // Save blob contents to a file.
        // The expression in a do binding must return unit.
        do
            use fileStream = File.OpenWrite($@"D:\Downloads\downloaded-myblob-{prefix}.txt") 
            blobToDownload.DownloadTo(fileStream)  |> ignore 

        // Delete blob 
        // Retrieve reference to a blob named "myblob.txt".
        let blobToDelete = container.GetBlobClient("myblob.txt")

        // Delete the blob.
        blobToDelete.Delete() |> ignore

        printfn "\n==DONE=="


    // Upload 200 blobs and list them in Async
    let demoAsyncBlobOperations () =
        let uploadBlobTo (containerClient: BlobContainerClient) (localFilePath: string) = 
            // https://zwpdbh.blob.core.windows.net/testblob1/ImportAndCopy.xml
            async {
                let fileName = Path.GetFileName(localFilePath)
                let blobClient = containerClient.GetBlobClient(fileName)    

                use stream = new MemoryStream()
                let bytes = System.Text.Encoding.ASCII.GetBytes("This is a sample text.")
                stream.Write(bytes, 0, bytes.Length)
                stream.Position <- 0
                return! blobClient.UploadAsync(stream) |> Async.AwaitTask
            }


        let listBlob (blobContainer: BlobContainerClient) = 
            asyncSeq {
                // How to handle IAsyncEnumerable<T> in F# ?
                // Use https://github.com/fsprojects/FSharp.Control.TaskSeq or FSharp.Control.AsyncSeq
                // TODO: see http://www.fssnip.net/869/title/An-IAsyncEnumerable-computation-expression-complete 
                // to understand how to use computation expression to solve this problem. 
                let resultSegment = blobContainer.GetBlobsAsync().AsPages(null, 10) |> AsyncSeq.ofAsyncEnum
                //resultSegment |> Seq.cast<Azure.Page<Models.BlobItem>>

                let! somethingToIgnore = 
                    resultSegment
                    |> AsyncSeq.iter (fun each -> 
                        // Convert handle IReadOnlyList : https://devonburriss.me/converting-fsharp-csharp/
                        // Here we simply change IReadOnlyList to F# list
                        each.Values 
                        |> List.ofSeq 
                        |> List.iter (fun eachBlobItem -> 
                            printfn $"{eachBlobItem.Name}"
                        )  
                    )
                yield somethingToIgnore
            } 

        let blobContainer = getBlobContainer ("testblob"  + System.DateTime.Now.ToString("yyyyMMdd-HHmmss"))

        // Upload blobs: https://learn.microsoft.com/en-us/azure/storage/blobs/storage-blob-upload
        seq {
            for each in [1..200] do 
                yield $"test{each}.txt"
        }
        |> Seq.map (uploadBlobTo blobContainer)
        |> Async.Parallel
        |> Async.Map (Seq.choose (fun response -> 
            match response.HasValue with 
            | true -> Some response.Value
            | false -> None 
        ))
        |> Async.RunSynchronously
        |> ignore

        // list blobs
        listBlob blobContainer
        |> AsyncSeq.toListSynchronously
        |> ignore

        printfn "\n==DONE=="
        

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