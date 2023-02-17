namespace Others.Snippets 
open Expecto

module SlideWindow = 

    module SubStringProblem = 
        let indexedCharsFromString s = 
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

            helper maxOne soFar (indexedCharsFromString s) 
            |> Map.toArray
            |> Array.sortBy (fun (_, i) -> i)
            //|> Array.map (fun (c, _) -> c)
            //|> System.String



        let demo () = 
            "GEEKSFORGEEKS" |> longestSubStr

    /// Try to use Activie Patterns to model slide window problem
    module SubStringProblemActivePattern = 

        /// Given a map where key is char, value is index position
        /// c is the key (a char), any keys (chars) with index <=i are removed
        /// Then, add (c, j) which is the new position for repeated character.
        let updateMapWithChar m c i j =            
            Map.toList m
            |> List.filter (fun (_, index) -> index > i)
            |> Map.ofList
            |> Map.add c j

        let indexedCharsFromString s = 
            s
            |> Seq.toList
            |> List.indexed
            |> List.map (fun (i, v) -> v, i)

        let createMapFromString s =
            s
            |> indexedCharsFromString
            |> Map.ofList

        /// Expand stage is only responsible to expand one character if possible.
        let (|Expand|_|) (currMap, restList) = 
            match restList with 
            | w::tail -> 
                let (c, j) = w 
                match currMap |> Map.tryFind c with 
                | None -> 
                    let updatedMap = 
                        currMap |> Map.add c j 
                    Some (updatedMap, tail)
                | _ -> 
                    None 
            | _ -> None

        /// Contract stage is only responsible to contract if possible
        let (|Contract|_|) (currMap, restList) = 
            match restList with 
            | w::tail -> 
                let (c, j) = w 
                match currMap |> Map.tryFind c with 
                | None -> 
                    Some (currMap, restList)
                | Some i -> 
                    let currMap' = updateMapWithChar currMap c i j 
                    Some (currMap', tail)
            | _ -> 
                None

        /// findSub is the patterns of keep doing expand..expand, contract, expand..expand ...until this process stoped
        /// So, the key is to make the result of expand and contract composible.
        /// Here, the necessary are the current Map and the rest input.
        let rec findSub (maxMap: Map<'a,'b> when 'a: comparison) currMap inputList = 
            match (currMap, inputList) with 
            | Expand (currMap, restList) -> 
                if currMap.Count > maxMap.Count then 
                    findSub currMap currMap restList 
                else 
                    findSub maxMap currMap restList
            | Contract (currMap, restList) ->
                findSub maxMap currMap restList
            | _ -> 
                maxMap

        let longestSubStr s = 
            let maxOne: Map<char, int>  = Map.empty
            let soFar: Map<char, int>  = Map.empty

            findSub maxOne soFar (indexedCharsFromString s) 
            |> Map.toArray
            |> Array.sortBy (fun (_, i) -> i)

        let demo () =
            "GEEKSFORGEEKS" |> longestSubStr


