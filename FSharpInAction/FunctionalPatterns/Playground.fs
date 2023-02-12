namespace FunctionalPatterns
module Playground =
    module MostFrequentWord = 
        open System.Text.RegularExpressions

        type Text = Text of string 

        let page1() = 
            List.replicate 1000 "hello world "
            |> List.reduce (+)
            |> Text

        let page2() = 
            List.replicate 1000 "goodbye world "
            |> List.reduce (+)
            |> Text

        let page3() = 
            List.replicate 1000 "foobar "
            |> List.reduce (+)
            |> Text

        let document() = 
            [page1(); page2(); page3()] 

        let addText s1 s2 = 
            Text (s1 + s2)

        let wordFreq (Text s) = 
            Regex.Matches(s, @"\S+")
            |> Seq.cast<Match>
            |> Seq.map (fun eachMatch -> eachMatch.ToString())
            |> Seq.groupBy id 
            |> Seq.map (fun (k, v) -> k, Seq.length v)
            |> Map.ofSeq

        let addMap m1 m2 = 
            let increment mapAcc k v = 
                match mapAcc |> Map.tryFind k with 
                | None -> 
                        mapAcc |> Map.add k v 
                | Some v' -> 
                    mapAcc |> Map.add k (v + v')

            m1 |> Map.fold increment m2

        let mostFrequentWord map = 
            let max (candindateW, maxCountSoFar) word count = 
                if count > maxCountSoFar then 
                    (word, count)
                else 
                    (candindateW, maxCountSoFar)

            map |> Map.fold max ("None", -1)


        let test = 
            // get the most frequent word from the merged smaller maps
            document() 
            |> List.map wordFreq
            |> List.reduce addMap
            |> mostFrequentWord
            |> printfn "Using map reduce, the most frequent and count is %A"


