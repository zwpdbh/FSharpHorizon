namespace Auth

module AuthSetting =
    type SubScription = { Name: string; Id: string }

    type ServicePrincipal =
        { 
            tenantId: string // The tenant id where the service principal is created from. It could be checked from Tenant properties --> Tenant ID.
            clientId: string // It is also the application id
            clientSecret: string option // NOTICE: Client secret values cannot be viewed, except for immediately after creation. So, make sure to save it immediately.
        }

    let subScriptionUsedByXscnWorkflowConsole =
        { 
            Name = "XTest Test Cluster STG Tenant Load Generators 8"
            Id = "33922553-c28a-4d50-ac93-a5c682692168" 
        }

    // It is the resource identifier of the resource you want. It is affixed with the .default suffix.
    // Here, the resource identifier is checked by
    // Azure AD --> App Registration -> ScenarioFramework -> Overview. Then check "Application ID URI". (If it is registered App, need append "/.default")
    let xscnScenarioScope = "https://scenariodeploymentsrestservice.azurewebsites.net/.default"
    let xscnworkflowconsoleScope = "https://microsoft.onmicrosoft.com/3b4ae08b-9919-4749-bb5b-7ed4ef15964d/.default"
    let azureScope = "https://management.azure.com/.default"
  
    let zwpdbhSP =
        { 
            tenantId = "72f988bf-86f1-41af-91ab-2d7cd011db47"
            // Usually there is an Object Id for a SP. But this is NOT the Object ID from the Application.
            // We can fetch it by: az ad sp show --id 2470ca86-3843-4aa2-95b8-97d3a912ff69 --query objectId
            clientId = "2470ca86-3843-4aa2-95b8-97d3a912ff69"
            clientSecret = None
        }


/// It currently support the following operations
/// 1. get certificate stored locally using thumprint
/// 2. get certificate stored locally suing subject name
/// 3. get SP's secret value stored in KeyVault
module KeyVault = 
    open Azure.Security.KeyVault.Secrets;
    open Azure.Identity
    open System
    open System.Security.Cryptography.X509Certificates

    let getLocalCertificateByThumbprint (thumbprint: string) = 
            use store = new X509Store(StoreName.My, StoreLocation.CurrentUser)
            store.Open(OpenFlags.OpenExistingOnly ||| OpenFlags.ReadOnly)
        
            let certificate = 
                store.Certificates
                |> Seq.filter(fun x -> 
                    x.Thumbprint.ToLower() = thumbprint.ToLower()
                    && DateTime.Now < x.NotAfter 
                    && DateTime.Now >= x.NotBefore
                )
                |> Seq.tryHead

            match certificate with 
            | Some cert -> cert 
            | None -> failwith $"Could not find a match for a certificate with thumbprint = '{thumbprint}'"

    let getLocalCertificateBySubject (subjectName: string) = 
            use store = new X509Store(StoreName.My, StoreLocation.CurrentUser)
            store.Open(OpenFlags.OpenExistingOnly ||| OpenFlags.ReadOnly)
        
            let certificate = 
                store.Certificates
                |> Seq.filter(fun x -> 
                    x.SubjectName.Name.ToLower() = ("cn=" + subjectName.ToLower())
                    && DateTime.Now < x.NotAfter 
                    && DateTime.Now >= x.NotBefore
                )
                |> Seq.tryHead

            match certificate with 
            | Some cert -> cert 
            | None -> failwith $"Could not find a match for a certificate with issueTo subject = '{subjectName}'"

    /// Fetch secret value (for example, my Service Princial (SP)'s secret) from KeyVault using SP
    /// 1. Need certificate installed on you local machine under (StoreName.My, StoreLocation.CurrentUser)
    /// 2. The same certificate is uploaded in SP: App registrations --> Your SP --> Certificates & secrets --> Certificates
    /// 3. Make sure your Key Vault's Access policies (NOT Access control -- IAM) is granted to SP (select by SP's client Id)
    let getSecretValue (secretName: string) (tenantId: string) (clientId: string) = 
        async {
            let cert = getLocalCertificateBySubject "zwpdbhREST"
            // Vault URI, checked from your Key Vault's overview
            let keyVaultUri = "https://zwpdbh.vault.azure.net/"
            let secretClient = new Lazy<SecretClient>(fun _ -> 
                new SecretClient(
                    new Uri(keyVaultUri), 
                    new ClientCertificateCredential(tenantId, clientId, cert))
            )
            return secretClient.Value.GetSecretAsync(secretName).Result.Value.Value 
            // secretClient.Value.GetSecretAsync(secretName).Value
        }
    
/// The credentials could be used by some Azure SDK. For example, for BlobServiceClient:
/// new BlobServiceClient(accountUri, new DefaultAzureCredential()), or
/// new BlobServiceClient(accountUri, clientCertificateCredential)
module Credential = 
    open Azure.Identity
    open Azure.Storage
    type CredentialOptionForStorageAccount = 
        | DefaultAzureCredential 
        | ServicePrincipal of tenantId: string * clientId: string * certificateSubject: string
        | StorageSharedKeyCredential of storageAccountName: string * storageAccountKey: string

    type CredentialForStorageAccount = 
        | DefaultCredential of DefaultAzureCredential
        | ClientCertificateCredential of ClientCertificateCredential
        | StorageForcedAccount of StorageSharedKeyCredential
        

    //private const string SubjectForRSRP = "rsrp-admin.xscenario.store.core.windows.net";
    //private const string SubjectForSP = "reader.xscenario.store.core.windows.net";
    //private const string AuthEndpoint = "https://login.windows.net";
    //private const string ResourceIdUri = "https://management.core.windows.net";

    //private string _preprodStorageLocation = "southcentralus";
    //private string _srpEndpoint = "https://srppreprodsouthcentralus.southcentralus.rsrp.storage.azure.com";
    //private string _srpStorageStamp = "sn4prdste16e";
    //private string _rsrpResourceGroup = "xstore";

    //private string _preprodPolicyServerEndpoint = "https://mockpolicyserver.azurewebsites.net";
    //private string _prodPolicyServerEndpoint = "https://management.azure.com";

    //private const string XScenarioTenantName = "XScenario";
    //private const string XScenarioTenantId = "2e5ee8cd-b069-473f-8629-b4010ae488be";
    //private const string XScenarioSubscriptionId = "d13eeeb5-2f98-4d41-a90e-c8cdf146b7cb";
    //private const string XScenarioClientId = "da1e3aa6-5eb8-46cb-922c-d75505dc911c";
    //private const string PrincipalId = "41fc944f-ef07-4cb3-800e-f8467bbe968f";

    //private X509Certificate2 _rsrpCertificate = null;
    //private X509Certificate2 _servicePrincipalCertificate = null;

    //private string _storageAccount;
    //private string _resourceGroup;

    //private bool _isProd;
    //private AuthorizationManagementClient _authorizationManagementClient;

    //private string _roleDefinitionId = Guid.NewGuid().ToString();
    //private string _roleAssignmentId = Guid.NewGuid().ToString();

    //private string _blobEndpoint = "";

    //private StorageManagementClient _preprodStorageManagementClient;
    //private ResourceGroupResource _prodResourceGroupResource;


    let getDefaultCredential () = 
        new DefaultAzureCredential()

    let getCredentialUsingSPCertificate tenantId clientId certificateSubjectName =
        new ClientCertificateCredential (
            tenantId,
            clientId,
            KeyVault.getLocalCertificateBySubject certificateSubjectName
        )

    let getStorageSharedKeyCredential storageAccountName storageAccountKey = 
        new StorageSharedKeyCredential(storageAccountName, storageAccountKey)

    let getConnectionString accountName accountKey endpointSuffix = 
        // ref: https://learn.microsoft.com/en-us/azure/storage/common/storage-configure-connection-string?toc=%2Fazure%2Fstorage%2Fblobs%2Ftoc.json&bc=%2Fazure%2Fstorage%2Fblobs%2Fbreadcrumb%2Ftoc.json#create-a-connection-string-for-an-explicit-storage-endpoint
        $"AccountName={accountName};AccountKey={accountKey};EndpointSuffix={endpointSuffix};DefaultEndpointsProtocol=https"

    let getCredential credentialType = 
        match credentialType with 
        | DefaultAzureCredential -> DefaultCredential (getDefaultCredential())
        | ServicePrincipal (tenantId, clientId, certificateSubject) -> 
            ClientCertificateCredential(getCredentialUsingSPCertificate tenantId clientId certificateSubject)
        | StorageSharedKeyCredential (storageAccountName, storageAccountKey) -> 
            StorageForcedAccount (getStorageSharedKeyCredential storageAccountName storageAccountKey)


        
/// A group of functions to obtain access token using SP
/// Especially, the SP's secret value is obtained from KeyVault (see KeyVault module).
module AuthToken = 
    open System
    open System.Net.Http
    open Thoth.Json.Net

    type AuthTokenResponse =
        {   token_type: string
            expires_in: DateTime
            access_token: string }

    let tokenExpireDecoder: Decoder<DateTime> = 
        Decode.int 
        |> Decode.andThen (fun expireInSeconds -> 
            let now = DateTime.Now;
            Decode.succeed (now.AddSeconds(expireInSeconds))
        )

    let authTokenDecoder: Decoder<AuthTokenResponse> = 
        Decode.object (
            fun get -> {
                AuthTokenResponse.token_type = get.Required.Field "token_type" Decode.string
                AuthTokenResponse.expires_in = get.Required.Field "expires_in" tokenExpireDecoder
                AuthTokenResponse.access_token = get.Required.Field "access_token" Decode.string
            }
        )

    let saveContentInfoFile fileName content =
        async {
            do! System.IO.File.WriteAllTextAsync(fileName, content) |> Async.AwaitTask 
        } |> Async.RunSynchronously

    let (|ParseAuthToken|_|) responseStr: Option<AuthTokenResponse> = 
        match responseStr |> (Decode.fromString authTokenDecoder) with 
        | Result.Ok authTokenResponse -> Some authTokenResponse
        | Result.Error _ -> None

    // ActivePattern which gets access_token using device-code oauth grant
    // See: https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-device-code
    let (|RequestAccessToken|_|) (sp: AuthSetting.ServicePrincipal, scope: string)=
        let client = new System.Net.Http.HttpClient()

        let clientSecret = 
            match sp.clientSecret with 
            | Some secret -> secret
            | None -> 
                KeyVault.getSecretValue "zwpdbhSPSecret" sp.tenantId sp.clientId
                |> Async.RunSynchronously
                  

        let authInfo =
            Map
                .empty
                .Add("client_id", sp.clientId)
                .Add("client_secret", clientSecret)
                .Add("scope", scope)
                .Add("grant_type", "client_credentials")

        let endpoint = $"https://login.microsoftonline.com/{sp.tenantId}/oauth2/v2.0/token"
        let req = new HttpRequestMessage((HttpMethod "post"), endpoint)
        req.Content <- new FormUrlEncodedContent(authInfo)

        let getAuthToken =
            async {
                let! response = client.SendAsync(req) |> Async.AwaitTask

                let! content =
                    response.Content.ReadAsStringAsync()
                    |> Async.AwaitTask

                match content with 
                | ParseAuthToken tokenResponse -> return Some tokenResponse
                | err -> 
                    printfn "ParseAuthToken from response failed, save it into file using saveContentInfoFile."
                    saveContentInfoFile @"D:\code\fsharp-programming\FSharpHorizen\WhyFSharp\FSharpDemo\AzureRelated\authResponse" err
                    return None
            }
        getAuthToken |> Async.RunSynchronously 


module AuthTokenAgent = 
    open AuthSetting
    open AuthToken

    type AuthTokenMsg = 
        | GetAccessToken of AsyncReplyChannel<Result<AuthTokenResponse, string>>
        | RequestNewToken of AsyncReplyChannel<Result<AuthTokenResponse, string>>

    type AuthTokenAgent(sp: ServicePrincipal, scope: string) = 
        let sp = sp             
        let agent = 
            MailboxProcessor.Start(fun inbox ->
                let rec loop (tokenResponseResult: Result<AuthTokenResponse, string> option) = 
                    async {       
                        let requestNewToken sp scope (chnl: AsyncReplyChannel<Result<AuthTokenResponse, string>>) = 
                            match sp, scope with 
                            | RequestAccessToken tokenResponse -> 
                                chnl.Reply (Result.Ok tokenResponse)
                                loop(Some (Result.Ok tokenResponse))
                            | _ -> 
                                loop(Some (Result.Error "RequestAccessToken failed"))    

                        let! msg = inbox.Receive()
                        match msg with 
                        | RequestNewToken chnl -> 
                            return! requestNewToken sp scope chnl                      
                        | GetAccessToken chnl  -> 
                            match tokenResponseResult with 
                            | Some result -> 
                                chnl.Reply result
                                return! loop(Some result)  // Don't forget this, always return! loop
                            | None -> 
                                return! requestNewToken sp scope chnl
                            return () 
                    }
                loop None // at start there is no token
            )

        member x.RequestNewToken () = 
            async {
                let! tokenResponse = agent.PostAndAsyncReply(fun chnl -> RequestNewToken(chnl))
                return tokenResponse 
            }

        member x.GetAccessToken () = 
            async {
                let! tokenResponse = agent.PostAndAsyncReply(fun chnl -> GetAccessToken(chnl))
                return tokenResponse 
            }          

    
/// Use zwpdbhSP to request access_token from default Azure scope.
/// The request access_token could be used to operate stand Azure APIs, like list Snapshots
module AzureAuthService = 
    open AuthTokenAgent
    let agent = new AuthTokenAgent(AuthSetting.zwpdbhSP, AuthSetting.azureScope)

    let getAuthToken () = 
        async {
            return! agent.GetAccessToken()
        }

    let getAccessToken () = 
        async {
            let! response =  agent.GetAccessToken()
            match response with 
            | Result.Ok authTokenResponse -> 
                return Result.Ok authTokenResponse.access_token
            | err -> 
                return Result.Error $"getAccessToken failed: {err}"
        }

/// Use zwpdbhSP to request access_token from XscnWorkflowConsole
/// The request access_token could be used to operate XscnWorkflowConsole APIs, like list workflow instances.
module XscnWorkflowConsoleAuthService = 
    open AuthTokenAgent

    let agent = new AuthTokenAgent(AuthSetting.zwpdbhSP, AuthSetting.xscnworkflowconsoleScope)

    let getAuthToken () = 
        async {
            return! agent.GetAccessToken()
        }

    let getAccessToken () = 
        async {
            let! response =  agent.GetAccessToken()
            match response with 
            | Result.Ok authTokenResponse -> 
                return Result.Ok authTokenResponse.access_token
            | err -> 
                return Result.Error $"getAccessToken failed: {err}"
        }


// https://microsoft.onmicrosoft.com/3b4ae08b-9919-4749-bb5b-7ed4ef15964d
module XscnScenarioAuthService = 
    open AuthTokenAgent

    let agent = new AuthTokenAgent(AuthSetting.zwpdbhSP, AuthSetting.xscnScenarioScope)

    let getAuthToken () = 
        async {
            return! agent.GetAccessToken()
        }

    let getAccessToken () = 
        async {
            let! response =  agent.GetAccessToken()
            match response with 
            | Result.Ok authTokenResponse -> 
                return Result.Ok authTokenResponse.access_token
            | err -> 
                return Result.Error $"getAccessToken failed: {err}"
        }