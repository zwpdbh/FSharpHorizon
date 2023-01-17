namespace AzureAPI

module HttpClient =
    open System 
    open System.Net.Http 
    open System.IO

    type RequestMethod = 
        | Get
        | Post 

    let client = new System.Net.Http.HttpClient()

    let buildQueryFromMap (parameters: Map<string, string>) =
        use content = new FormUrlEncodedContent(parameters)
        let stream = content.ReadAsStream()
        let reader = new StreamReader(stream)
        reader.ReadToEnd()

    let handleResponseAsync req = 
        async {
            let! response = client.SendAsync(req) |> Async.AwaitTask

            return!
                response.Content.ReadAsStringAsync()
                |> Async.AwaitTask
        }

    let sendRequestAsync (method: RequestMethod)  (endpoint: string) (someHeaders: Map<string, string> option) (someQueryParameters: Map<string, string> option) = 
        let req = new HttpRequestMessage()
        match method with 
        | Get -> 
            req.Method <- HttpMethod.Get
        | Post -> 
            req.Method <- HttpMethod.Post

        match someQueryParameters with 
        | Some parameters -> 
            req.RequestUri <- 
                Uri(endpoint + "?" + (buildQueryFromMap parameters))
        | None -> 
            req.RequestUri <- 
                Uri(endpoint)

        match someHeaders with 
        | Some headers -> 
            req.Content <- new FormUrlEncodedContent(headers)
        | None -> 
            ()

        handleResponseAsync req
    

