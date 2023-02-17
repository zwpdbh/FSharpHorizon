namespace Others.FromInterview

open Others
open Expecto

module Polymer = 
    module Problem01 = 
        type Position = Pair of int * int 
        type Text = PlainString of string 
        type Document = Text of Map<Position, Text>

        type EditMessage = 
            | AddText of AsyncReplyChannel<Result<Position, string>> * Text 
            | RemoveText of AsyncReplyChannel<Result<Position, string>> * Position

        let size (PlainString text) =
            String.length text 

        type Editor (document: Document) = 
            let (Text document) = document 
            let agent = 
                MailboxProcessor.Start(fun inbox -> 
                    let rec loop (state: Map<Position, Text>) =
                        async {
                            let! msg = inbox.Receive()
                            match msg with 
                            | AddText (chnnl, text) ->
                                
                                let lastOne = 
                                    state 
                                    |> Map.toList
                                    |> List.sortBy (fun (Pair (x, _), _) -> x)
                                    |> List.tryHead 

                                let textLengh = size text

                                let position, state' =
                                    match lastOne with 
                                    | Some (position, text) -> 
                                        let Pair(i, j) = position
                                        let nextP = j + 1          
                                        let position = (Pair (nextP, nextP + textLengh))
                                        position, state |> Map.add position text
                                    | None ->
                                        let position = (Pair (0, size text))
                                        position, state |> Map.add position text

                                chnnl.Reply (Result.Ok position)
                                return! loop(state')
                            | RemoveText (chnnl, position) -> 
                                let state' = state |> Map.remove position
                                chnnl.Reply (Result.Ok position)
                                return! loop(state')
                        }
                    loop(document)
                ) 

            member x.Addtext (text: Text) = 
                async {
                    return!  agent.PostAndAsyncReply(fun chnnl -> AddText(chnnl, text))
                }

            member x.RemoveText (position: Position) =
                async {
                    return! agent.PostAndAsyncReply(fun chnnl -> RemoveText(chnnl, position))
                }

        let demo () = 
            let document: Map<Position, Text> = Map.empty
            let editor = new Editor(Text document)

            async {
                return! editor.Addtext (PlainString "something")
            }
        
        let test00 = 
            testCase "test00"
            <| fun _ -> 
                Expect.isTrue true "00"

        let demo () = 
            0
    [<Tests>]
    let tests = testList "FromInterview.Polymer" [Problem01.test00] 



