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
            S1 = "abcde" |> List.ofSeq
            S2 = "ace" |> List.ofSeq
            Expected = "ace" 
        |}

        let input02 = {|
            S1 = "ACCGGTCGAGTGCGCGGAAGCCGGCCGAA" |> List.ofSeq
            S2 = "GTCGTTCGGAATGCCGTTGCTCTGTAAA" |> List.ofSeq
            Expected = "GTCGTCGGAAGCCGGCCGAA"
        |}

        let lcs (s1: char list) (s2: char list) = 
            let rec helper s1 s2 acc = 
                match s1, s2 with 
                | [], _ -> s2 @ acc 
                | _, [] -> s1 @ acc 
                | x1::tail01, x2::tail02 -> 
                    match x1 = x2 with 
                    | true -> x1 :: acc 
                    | false -> 
                        let lcs01 = helper tail01 s2 acc
                        let lcs02 = helper s1 tail02 acc
                        if lcs01.Length >= lcs02.Length then 
                            lcs01 
                        else 
                            lcs02 

            helper s1 s2 []
            |> List.rev
            |> Array.ofList
            |> System.String.Concat


        let test01 = 
            testCase "Problem02.1"
            <| fun _ ->
                let result = lcs input01.S1 input01.S2
                Expect.equal result input01.Expected ""
        
        

    [<Tests>]
    let tests = 
        testList "From Optiver " [Problem01.test01; Problem01.test02; Problem01.test03; Problem01.test04; 
            Problem02.test01
        ]