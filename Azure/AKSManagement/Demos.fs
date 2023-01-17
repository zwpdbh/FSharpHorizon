namespace AKSManagement
module Demos = 
    let demoListSnapshot () : string = 
        async {
            let! accessTokenResponse = AzureAuth.AuthService.getAccessToken()
            match accessTokenResponse with 
            | Result.Ok accessToken -> 
                return! AzureAPI.Snapshot.listAllSnapshots "33922553-c28a-4d50-ac93-a5c682692168" accessToken    
            | Result.Error err -> 
                printfn "No access token"
                //failwith err 
                return err 
        } |> Async.RunSynchronously


