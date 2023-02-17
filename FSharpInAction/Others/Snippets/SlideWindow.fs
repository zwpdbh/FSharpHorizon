namespace Others.Snippets 
open Expecto

module SlideWindow = 

    module SubStringProblem = 
        let charsFromString s = 
            s
            |> Seq.toList
            |> List.indexed
            |> List.map (fun (k, v) -> v, k)

        let genFilterFunc n = 
            let filterFun k _ = 
                k > n 
            filterFun 

        // Find the longest substring without repeating characters
        let longestSubStr (s: string) = 
            let rec helper (maxOne: Map<'a,'b> when 'a: comparison and 'b: comparison) soFar input = 
                match input with 
                | w:: tail -> 
                    let (c, i) = w 
                    match soFar |> Map.tryFind c with 
                    | None -> 
                        let updatedSofar = soFar |> Map.add c i 
                        if updatedSofar.Count > maxOne.Count then 
                            helper updatedSofar updatedSofar tail 
                        else 
                            helper maxOne updatedSofar tail 
                    | Some j -> 
                        // Need to update the subSofor to use the latest encouted character. 
                        // But need to remove all characters ahead of repeated one (inclusive)
                        let updatedSofar = 
                            soFar 
                            |> Map.toList
                            |> List.filter (fun (_, index) -> index > j)
                            |> Map.ofList
                            |> Map.add c i 
                        helper maxOne updatedSofar tail 
                | _ ->
                    maxOne

            let maxOne: Map<char, int>  = Map.empty
            let soFar: Map<char, int>  = Map.empty
            helper maxOne soFar (charsFromString s) 
            |> Map.toArray
            |> Array.sortBy (fun (_, i) -> i)
            //|> Array.map (fun (c, _) -> c)
            //|> System.String



        let demo () = 

            "GEEKSFORGEEKS" |> longestSubStr


    module SubStringProblemActivePattern = 


