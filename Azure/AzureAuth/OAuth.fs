namespace AzureAuth

module AuthSetting =
    type SubScription = { Name: string; Id: string }

    type ServicePrincipal =
        { 
            tenantId: string // The tenant id where the service principal is created from. It could be checked from Tenant properties --> Tenant ID.
            clientId: string // It is also the application id
            clientSecret: string option // NOTICE: Client secret values cannot be viewed, except for immediately after creation. So, make sure to save it immediately.
        }

    let deploymentServiceSubScription =
        { 
            Name = "XTest Test Cluster STG Tenant Load Generators 8"
            Id = "33922553-c28a-4d50-ac93-a5c682692168" 
        }

    //// It is the resource identifier of the resource you want. It is affixed with the .default suffix.
    //// Here, the resource identifier is checked by
    //// Azure AD --> App Registration -> ScenarioFramework -> Overview. Then check "Application ID URI".
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


module AuthService = 
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
                    let requestNewToken sp scope (chnl: AsyncReplyChannel<Result<AuthTokenResponse, string>>) = 
                        match sp, scope with 
                        | RequestAccessToken tokenResponse -> 
                            chnl.Reply (Result.Ok tokenResponse)
                            loop(Some (Result.Ok tokenResponse))
                        | _ -> 
                            loop(Some (Result.Error "RequestAccessToken failed"))    
                           
                    async {
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
