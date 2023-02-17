namespace Others.Snippets

module ProcessText =
    open Others

    let doc01 = Common.readText @"Snippets\data\words01.txt"
    let doc02 = Common.readText @"Snippets\data\words02.txt"
    let doc03 = Common.readText @"Snippets\data\words02.txt"

    module Frequence = 
        open System.Text.RegularExpressions 

        type Text = Text of string 

        let wordFreq (Text s) = 
            Regex.Matches(s, @"\S+")
            |> Seq.cast<Match>
            |> Seq.map (fun e -> e.ToString ())
            |> Seq.groupBy id 
            |> Seq.map (fun (k, v) -> k, Seq.length v)
            |> Map.ofSeq

        let addMap m1 m2 = 
            /// helper function used in fold to Merge frequences.  
            let incremental mapSofar word count = 
                match mapSofar |> Map.tryFind word with 
                | None -> 
                    mapSofar |> Map.add word count 
                | Some count' ->
                    mapSofar |> Map.add word (count + count')
            
            m1 |> Map.fold incremental m2 

        let mostFrequence m = 
            let max (candidateWord, maxCountSofar) word count =
                if count > maxCountSofar then 
                    (word, count)
                else 
                    (candidateWord, maxCountSofar)

            m |> Map.fold max ("None", 0)


        let demoMostFrequence () = 
            [doc01; doc02; doc03]
            |> List.map (fun e -> Text e)
            |> List.map wordFreq 
            |> List.reduce addMap 
            |> mostFrequence




