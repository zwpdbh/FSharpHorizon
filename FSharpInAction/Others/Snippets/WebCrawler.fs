namespace Others.Snippets

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Net
open System.Text.RegularExpressions

module Helpers =

  type Message =
    | Done
    | Mailbox of MailboxProcessor<Message>
    | Stop
    | Url of string option

  // Gates the number of crawling agents.
  [<Literal>]
  let Gate = 5

  // Extracts links from HTML.
  let extractLinks html =
    let pattern1 =
      "(?i)href\\s*=\\s*(\"|\')/?((?!#.*|/\B|mailto:|location\.|javascript:)[^\"\']+)(\"|\')"

    let pattern2 = "(?i)^https?"

    let links =
      [ for x in Regex(pattern1).Matches(html) do
          yield x.Groups.[2].Value ]
      |> List.filter (fun x -> Regex(pattern2).IsMatch(x))

    links

  // Fetches a Web page.
  let fetch (url: string) =
    try
      let req = WebRequest.Create(url) :?> HttpWebRequest
      req.UserAgent <- "Mozilla/5.0 (Windows; U; MSIE 9.0; Windows NT 9.0; en-US)"
      req.Timeout <- 5000
      use resp = req.GetResponse()
      let content = resp.ContentType
      let isHtml = Regex("html").IsMatch(content)

      match isHtml with
      | true ->
        use stream = resp.GetResponseStream()
        use reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        Some html
      | false -> None
    with
    | _ -> None

  let collectLinks url =
    let html = fetch url

    match html with
    | Some x -> extractLinks x
    | None -> []




module WebCrawler =
  open Helpers

  let crawl url limit =
    // Concurrent queue for saving collected urls.
    let q = ConcurrentQueue<string>()

    // Holds crawled URLs.
    let set = HashSet<string>()

    let supervisor =
      MailboxProcessor.Start (fun x ->
        let rec loop run =
          async {
            let! msg = x.Receive()

            match msg with
            | Mailbox (mailbox) ->
              let count = set.Count

              if count < limit - 1 && run then
                let url = q.TryDequeue()

                match url with
                | true, str ->
                  if not (set.Contains str) then
                    let set' = set.Add str
                    mailbox.Post <| Url(Some str)
                    return! loop run
                  else
                    mailbox.Post <| Url None
                    return! loop run

                | _ ->
                  mailbox.Post <| Url None
                  return! loop run
              else
                mailbox.Post Stop
                return! loop run
            | Stop -> return! loop false
            | _ ->
              printfn "Supervisor is done."
              (x :> IDisposable).Dispose()
          }

        loop true)


    let urlCollector =
      MailboxProcessor.Start (fun y ->
        let rec loop count =
          async {
            let! msg = y.TryReceive(6000)

            match msg with
            | Some message ->
              match message with
              | Url u ->
                match u with
                | Some url ->
                  q.Enqueue url
                  return! loop count
                | None -> return! loop count
              | _ ->
                match count with
                | Gate ->
                  supervisor.Post Done
                  (y :> IDisposable).Dispose()
                  printfn "URL collector is done."
                | _ -> return! loop (count + 1)
            | None ->
              supervisor.Post Stop
              return! loop count
          }

        loop 1)

    /// Initializes a crawling agent.
    let crawler id =
      MailboxProcessor.Start (fun inbox ->
        let rec loop () =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Url x ->
              match x with
              | Some url ->
                let links = collectLinks url
                printfn "%s crawled by agent %d." url id

                for link in links do
                  urlCollector.Post <| Url(Some link)

                supervisor.Post(Mailbox(inbox))
                return! loop ()
              | None ->
                supervisor.Post(Mailbox(inbox))
                return! loop ()
            | _ ->
              urlCollector.Post Done
              printfn "Agent %d is done." id
              (inbox :> IDisposable).Dispose()
          }

        loop ())

    // Spawn the crawlers.
    let crawlers =
      [ for i in 1..Gate do
          yield crawler i ]

    // Post the first messages.
    crawlers.Head.Post <| Url(Some url)

    crawlers.Tail
    |> List.iter (fun ag -> ag.Post <| Url None)

  let demo () =
    // Example:
    crawl "http://news.google.com" 25