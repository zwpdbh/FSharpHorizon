namespace Others.Snippets 

module WsClient = 
    // Use this address to test: 
    // https://www.piesocket.com/websocket-tester

    // TBD: read https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_servers
    module WebSocketClient = 
        open Websocket.Client
        open System

        let endpoint = "wss://stream.binance.us:9443/ws/"

        let tradeUrl (symbol: string) = 
            endpoint + symbol.ToLower() + "@trade" 

        // It received something like:
        //{"e":"trade","E":1678281305803,"s":"ETHUSD","t":44172879,"p":"1549.38000000","q":"0.05300000","b":1478766161,"a":1479008752,"T":1678281305802,"m":true,"M":true}
        let processMessage (msg: ResponseMessage) = 
            printfn $"Received message: {msg}"

        let processReconnection (msg: Models.ReconnectionInfo) = 
            printfn $"Reconnection happened, type: {msg.Type}"

        let processDisconnection (msg: DisconnectionInfo) = 
            printfn $"disconnected: {msg}"

        let rec receiveMessage (ws: WebsocketClient) =       
            async {
                do! receiveMessage ws
            }

        let runClient (ws: WebsocketClient) = 
            async {
                
                do! ws.Start() |> Async.AwaitTask
                do! [
                        receiveMessage ws 
                    ] |> Async.Parallel |> Async.Ignore
            }


        let startStreaming (symbol: string) = 
            let ws = new WebsocketClient(new Uri(tradeUrl symbol))

            ws.ReconnectTimeout <- TimeSpan.FromSeconds(30)
            ws.DisconnectionHappened.Subscribe(processDisconnection) |> ignore
            ws.ReconnectionHappened.Subscribe(processReconnection) |> ignore
            ws.MessageReceived.Subscribe(processMessage) |> ignore

            runClient ws |> Async.RunSynchronously


    module WebSocketsClient = 
        open System
        open System.Net.WebSockets
        open System.Threading

        let endpoint = "wss://stream.binance.us:9443/ws/"
        let url = endpoint + "ethusd" + "@trade"

        let ws = new ClientWebSocket()

        let rec receiveMessage (ws: ClientWebSocket) (buffer: byte[]) =
            // recieved something like this:
            // {"e":"trade","E":1678280510810,"s":"ETHUSD","t":44172601,"p":"1553.05000000","q":"0.15630000","b":1479031415,"a":1479031202,"T":1678280510809,"m":false,"M":true}
            async {
                let! result = ws.ReceiveAsync(ArraySegment buffer, CancellationToken.None) |> Async.AwaitTask
                if result.MessageType = WebSocketMessageType.Close then
                    do! ws.CloseAsync(WebSocketCloseStatus.NormalClosure, "", CancellationToken.None) |> Async.AwaitTask
                else
                    let message = System.Text.Encoding.UTF8.GetString(buffer.[..result.Count])
                    printfn "Received: %s" message // print the received message
                    return! receiveMessage ws buffer // loop to receive more messages
            }
        
        let rec sendMessage (ws: ClientWebSocket) =
            async {
                let input = Console.ReadLine() // read user input from console
                if input <> "exit" then // exit if user types "exit"
                    let buffer = System.Text.Encoding.UTF8.GetBytes(input)
                    do! ws.SendAsync(ArraySegment buffer, WebSocketMessageType.Text, true, CancellationToken.None) |> Async.AwaitTask // send the input as a text message
                    return! sendMessage ws // loop to send more messages
            }
        
        let runClient () =
            async {
                do! ws.ConnectAsync(Uri url, CancellationToken.None) |> Async.AwaitTask // connect to the websocket server
                printfn "Connected to %s" url 

                let buffer = Array.zeroCreate 1024 // create a buffer for receiving messages
                do! 
                    [
                        receiveMessage ws buffer
                        //sendMessage ws
                    ] |> Async.Parallel |> Async.Ignore // run both receive and send functions in parallel 
            }
        
        let demo () = 

            runClient () |> Async.RunSynchronously // run the client asynchronously


