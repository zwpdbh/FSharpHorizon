
open DemoREPerformance
open FunAndProfit

//let n = 5000 * 5

//timeRegulareExpression n |> printfn "regulare expression takes: %A" 
//timeStringContain n |> printfn "string contains takes: %A" 


open System.IO


let speedFile = @"D:\code\fsharp-programming\FSharpHorizen\FSharpInAction\Interviews\Optiver\speed.txt"

let loadSpeedRecord filePath = 
    File.ReadAllLines filePath 
    |> List.ofArray 
    |> List.filter (fun x -> x.Trim() <> "")
    |> List.map (fun x -> int x)

let rec expandUntailOneKm accDis accList recordList=
    match accDis < 1000, recordList with 
    | true, x::tail -> 
        expandUntailOneKm (accDis + x) (x::accList) tail 
    | false, x::tail -> 
        accDis, accList, recordList 
    | _, _ -> 
        accDis, accList, recordList 

//let rec contractUntilOneKm accDis recordsList = 
//    match recordsList with 
//    | x::(y::_ as tail)-> 
//        match accDis - x > 1000, accDis - x - y < 1000 with
//        | true, true -> 
//            accDis - x, tail 
//        | _ -> 
//            contractUntilOneKm (accDis - x) tail 
//    | _ -> 
//        accDis, recordsList 

let rec contractUntilOneKm accDis recordsList = 
    match accDis >= 1000, recordsList with 
    | true, x::tail when (accDis - x) >= 1000 -> 
        contractUntilOneKm (accDis - x) tail 
    | true, _ -> 
        accDis, recordsList 
    | false, _ ->
        failwith "when contract, accDis should always >= 1000"

let (|GetJustOneKm|_|) recordList = 
    let accDis, accList, restList = 
        recordList
        |> expandUntailOneKm 0 []    

    if accDis < 1000 then 
        None 
    else 
        let dis, disList = contractUntilOneKm accDis (accList |> List.rev)
        Some (dis, disList, restList)


let demo () = 
    let recordList = (loadSpeedRecord speedFile)
    let rec findAllOneKmGroup recordList = 
        match recordList with
        | GetJustOneKm (dist, distList, rest) -> 
            printfn $"dist = {dist}"
            findAllOneKmGroup (distList[1..] @ rest)
        | _ -> 
            printfn "Couldn't find more, use what is left"
            printfn $"the rest of records are: ${recordList}, there total distance is: {recordList |> List.sum}"

    findAllOneKmGroup recordList



demo()


//let result = 
//    maxAverageSpeed (loadSpeedRecord speedFile)

//for each in result do 
//    printfn $"{each} : {List.sum each}"