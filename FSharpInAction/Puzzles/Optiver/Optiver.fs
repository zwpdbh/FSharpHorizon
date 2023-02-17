namespace Puzzles

open Expecto
open System
open System.IO

module Optiver = 

    module Problem01 = 
        
        let workingDirectory = Environment.CurrentDirectory;
        let projectFolder = Directory.GetParent(workingDirectory).Parent.Parent.FullName

        let speedRecordsFile = @"Optiver\speed.txt"

        let loadSpeedRecord filePath = 
            Common.readAllLines filePath 
            |> List.ofArray 
            |> List.filter (fun x -> x.Trim() <> "")
            |> List.map (fun x -> int x)

        // slide window expand 
        let rec expandUntailOneKm accDist accList restList=
            match accDist < 1000, restList with 
            | true, x::tail -> 
                expandUntailOneKm (accDist + x) (x::accList) tail 
            //| false, _ -> 
            //    accDis, accList, recordList 
            | _, _ -> 
                accDist, accList |> List.rev, restList 

        // slide window contract
        let rec contractUntilOneKm accDist recordsList = 
            match accDist >= 1000, recordsList with 
            | true, x::tail when (accDist - x) >= 1000 -> 
                contractUntilOneKm (accDist - x) tail 
            | true, _ -> 
                accDist, recordsList
            | false, _ ->
                failwith "when contract, accDis should always >= 1000"

        // Use active pattern to treat slide window (left and right pointer) expand and contract as one atomic operation    
        let (|GetJustOneKm|_|) recordList = 
            let accDist, accList, restList = 
                recordList
                |> expandUntailOneKm 0 []    

            if accDist < 1000 then 
                None 
            else 
                let dis, disList = contractUntilOneKm accDist accList
                Some (dis, disList, restList)

        // Find the maximum average speed over 1 km
        // Each element in the list is a distance run in 5 mins
        let maxAverageSpeed (records: int list) = 
            let rec helper records maxSpeed= 
                match records with 
                | GetJustOneKm (dist, distList, rest) -> 
                    let currSpeed = (float dist) / (float distList.Length * 5.0)
                    if currSpeed >= maxSpeed then 
                        helper (distList[1..] @ rest) currSpeed
                    else 
                        helper (distList[1..] @ rest) maxSpeed
                | _ -> 
                    maxSpeed
            helper records 0.0

        let input = [
            34
            14
            28
            200
            500
            100
            10
            17
            28
            36
            19
            700
            38
            39
            36
            14
            10
        ]

        // The list we got when we first move right pointer to distance >= 1000
        let expandGroup = [
            34
            14
            28
            200
            500
            100
            10
            17
            28
            36
            19
            700
        ]

        // The list we got when we move left pointer until its distance can not smaller than 1000
        let contractGroup = [
            500
            100
            10
            17
            28
            36
            19
            700
        ]

        let test01 = 
            testCase "Problem01.1: test expand works" 
            <| fun _ -> 

                let accDist, accList, restList = expandUntailOneKm 0 [] input
                Expect.equal accDist 1686 "test 1"
                Expect.equal (accList |> List.last) 700 "test 2"
                Expect.equal (restList.Head) 38 "test 3"

        let test02 = 
            testCase "Problem01.2: test contract"
            <| fun _ -> 

                let dist, distList = contractUntilOneKm (expandGroup |> List.sum) expandGroup 
                Expect.equal dist (contractGroup |> List.sum) "tes 1"
                Expect.sequenceEqual distList contractGroup "test 2"

        let test03 = 
            testCase "Problem01.3: GetJustOneKm"
            <| fun _ -> 

                match input with 
                | GetJustOneKm (dist, distList, restList) -> 
                    Expect.sequenceEqual distList contractGroup "test 0 "
                    Expect.equal dist (contractGroup |> List.sum) "test 1"
                    Expect.equal (restList.Length) 5 "test 2"
                    Expect.equal (restList |> List.head) 38 "test 3"
                    Expect.equal (restList |> List.last) 10 "test 4"
                | _ -> 
                    failwith "something wrong"

        let test04 = 
            testCase "Problem01.4: GetMaxAvgSpeed"
            <| fun _ -> 
                let maxSpeed = 
                    loadSpeedRecord speedRecordsFile
                    |> maxAverageSpeed

                Expect.equal maxSpeed 35.25 "test 0"


    module Problem02 = 
    // Longest common sequence
    // It is the same as: https://leetcode.com/problems/longest-common-subsequence

        let input01 = {|
            S1 = "abcde" 
            S2 = "ace" 
            Expected = "ace" 
        |}

        let input02 = {|
            S1 = "ACCGGTCGAGTGCGCGGAAGCCGGCCGAA"
            S2 = "GTCGTTCGGAATGCCGTTGCTCTGTAAA" 
            Expected = "GTCGTCGGAAGCCGGCCGAA"
        |}

        let lcsV1 (s1: string) (s2: string) = 
            let rec helper s1 s2 acc = 
                match s1, s2 with 
                | [], _ -> acc
                | _, [] -> acc
                | x1::tail01, x2::tail02 -> 
                    match x1 = x2 with 
                    | true ->   
                        helper tail01 tail02 (x1::acc)
                    | false -> 
                        let lcs01 = helper tail01 s2 [] 
                        let lcs02 = helper s1 tail02 [] 
                        if lcs01.Length >= lcs02.Length then 
                            lcs01 @ acc  
                        else 
                            lcs02 @ acc

            helper (s1 |> List.ofSeq) (s2 |> List.ofSeq) []
            |> List.rev
            |> Array.ofList
            |> System.String.Concat


        let test01 = 
            testCase "Problem02.1"
            <| fun _ ->
                let result = lcsV1 input01.S1 input01.S2
                Expect.equal result input01.Expected "test 01"

                Expect.equal (lcsV1 "abcdefg" "adfg") "adfg" "test 02"

        
        type Choice = 
            | Both
            | First
            | Second

        let rec getTrace s1 m s2 n (trace: Map<int * int, Choice>) traceResult =         
            match s1, s2 with 
            | [], _ -> 
                traceResult
            | _, [] ->
                traceResult
            | h1 :: tail1, h2 :: tail2 ->
                match trace.TryFind(m, n) with 
                | Some v ->
                    match v with 
                    | Both ->
                        // Only in both case, we take it into lcs
                        getTrace tail1 (m + 1) tail2 (n + 1) trace (h1::traceResult)
                    | First -> 
                        getTrace tail1 (m + 1) s2 n trace traceResult
                    | Second -> 
                        getTrace s1 m tail2 (n+1) trace traceResult
                | None -> 
                    failwith "Should not happened because each step we follow the trace"


        let lcsV2 (s1: string) (s2: string) = 
            let mutable lookup: Map<int * int, int> = Map.empty
            let mutable trace: Map<int * int, Choice> = Map.empty
            let rec helper s1 m s2 n =
                match lookup.TryFind(m, n) with 
                | Some x -> x 
                | None -> 
                    match s1, s2 with 
                    | [], _ -> 
                        lookup <- lookup.Add((m, n), 0)       
                        0
                    | _, [] -> 
                        lookup <- lookup.Add((m, n), 0)     
                        0
                    | h1::tail1, h2::tail2 -> 
                        match h1 = h2 with 
                        | false -> 
                            let l1 = helper tail1 (m+1) s2 n 
                            let l2 = helper s1 m tail2 (n+1) 
                            if l1 >= l2 then 
                                lookup <- lookup.Add((m, n), l1)
                                trace <- trace.Add((m, n), First)
                                l1 
                            else 
                                lookup <- lookup.Add((m, n), l2)
                                trace <- trace.Add((m, n), Second)
                                l2
                        | true -> 
                            let l = 1 + helper tail1 (m + 1) tail2 (n + 1) 
                            lookup <- lookup.Add((m, n), l)
                            trace <- trace.Add((m, n), Both)
                            l 
            
            let len = 
                helper (s1 |> List.ofSeq) 0 (s2 |> List.ofSeq) 0 


            let str = 
                getTrace (s1 |> List.ofSeq) 0 (s2 |> List.ofSeq) 0 trace []
                |> List.rev 
                |> Array.ofList
                |> System.String.Concat
            len, str

        let test02 = 
            testCase "Problem02.2"
            <| fun _ ->
                //let result = lcsV1 input01.S1 input01.S2
                //Expect.equal result input01.Expected "test 01"
                let (x, n) = lcsV2 input01.S1 input01.S2 
                Expect.equal x input01.Expected.Length "test 01.1" 
                Expect.equal n input01.Expected "test 01.2"

                let (len, str) = lcsV2 input02.S1 input02.S2
                Expect.equal len input02.Expected.Length "test 02.1" 
                Expect.equal str input02.Expected "test 02.2"

    [<Tests>]
    let tests = 
        testList "From Optiver " [
            Problem01.test01; Problem01.test02; Problem01.test03; Problem01.test04; 
            Problem02.test01
            Problem02.test02
        ]