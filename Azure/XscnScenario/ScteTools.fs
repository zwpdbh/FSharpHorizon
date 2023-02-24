namespace XscnScenario



module ScteTools =
    open Azure.Storage
    open Azure.Storage.Blobs
    open Azure.Storage.Blobs.Models
    open System
    open System.IO


    let checkBlobExportedBySCTE () = 
        let accountName = "bbpxscnams09pe02ax"
        //let accountKey = "<see the corresponding account key when create ForcedAccount

        let serviceUri = new Uri("https://bbpxscnams09pe02ax.blob.preprod.core.windows.net")
        let credential = Auth.Credential.getStorageSharedKeyCredential accountName accountKey

        // Create a client that can authenticate with a connection string            
        let blobServiceClient = new BlobServiceClient(serviceUri, credential)

        let blobContainerName = "aaad00a53dfi00009c86a84d-8766-468d-918b-02cb5fc95219container"
        let containerClient = blobServiceClient.GetBlobContainerClient(blobContainerName)

        containerClient.GetBlobs()
        |> Seq.filter (fun eachBlobItem -> eachBlobItem.Name.Contains("aaad00a53dfi000097blob"))
        |> Seq.map (fun i -> 
            
            let metadata = 
                seq {
                    for each in i.Metadata do 
                        yield each.Key + " " +  each.Value
                }
                |> String.concat " "
            if String.IsNullOrEmpty metadata then 
                $"name= {i.Name}, snapshot={i.Snapshot}"
            else 
                $"name= {i.Name}, snapshot={i.Snapshot}, metadata={metadata}"   
        )


    //let getAccountKeyDemo () = 
    //    let accountName = "bbpxscnams09pe02ax"
    //    let tenantId = "2e5ee8cd-b069-473f-8629-b4010ae488be"
    //    let clientId = "da1e3aa6-5eb8-46cb-922c-d75505dc911c"
    //    Auth.KeyVault.getSecretValue accountName tenantId clientId
    //    |> Async.RunSynchronously
