namespace RestAPI

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
            response.EnsureSuccessStatusCode |> ignore 

            return!
                response.Content.ReadAsStringAsync()
                |> Async.AwaitTask
        }

    let getRequestAsync (endpoint: string) (someHeaders: Map<string, string> option) (someQueryParameters: Map<string, string> option) = 

        let req = new HttpRequestMessage()
        req.Method <- HttpMethod.Get

        match someHeaders with 
        | Some headers -> 
            headers 
            |> Map.toSeq 
            |> Seq.iter (fun (k, v) -> 
                req.Headers.Add(k, v)
            )
        | None -> 
            ()

        match someQueryParameters with 
        | Some parameters -> 
            req.RequestUri <- 
                Uri(endpoint + "?" + (buildQueryFromMap parameters))
        | None -> 
            req.RequestUri <- 
                Uri(endpoint)



        handleResponseAsync req


    let postRequestAsync (endpoint: string) (someHeaders: Map<string, string> option) (someQueryParameters: Map<string, string> option) (someBody: Map<string, string> option) = 

        let req = new HttpRequestMessage()
        req.Method <- HttpMethod.Get

        match someHeaders with 
        | Some headers -> 
            headers 
            |> Map.toSeq 
            |> Seq.iter (fun (k, v) -> 
                req.Headers.Add(k, v)
            )
        | None -> 
            ()

        match someQueryParameters with 
        | Some parameters -> 
            req.RequestUri <- 
                Uri(endpoint + "?" + (buildQueryFromMap parameters))
        | None -> 
            req.RequestUri <- 
                Uri(endpoint)

        match someBody with 
        | Some body -> 
            req.Content <- new FormUrlEncodedContent(body)
        | None -> 
            ()

        handleResponseAsync req

