namespace Puzzle
open Expecto

module BasicMap = 
    let test01 = 
        testCase "Map"
        <| fun _ -> 
            let testMap = 
                Map
                    .empty
                    .Add("1", 1)
                    .Add("2", 2)
        
            Expect.isTrue (testMap.ContainsKey("1")) "01 map contains key"

            let updatedMap01 = testMap.Remove("1")
            Expect.isFalse (updatedMap01.ContainsKey("1")) "02 map remove key"

            let updatedMap02 = testMap.Add("3", 3)
            let updatedMap03 = updatedMap01.Add("3", 3)

            Expect.equal (updatedMap02.Count) 3 "03 map element size"
            Expect.equal (updatedMap03.Count) 2 "04 map element size"


    [<Tests>]
    let tests = 
        testList "Commonly used F# basics" [test01]


module RandomSelection = 
    open System
    // learn from https://stackoverflow.com/questions/33312260/how-can-i-select-a-random-value-from-a-list-using-f
    let r = System.Random()

    let selectRandom3 () = 
        let shuffle next xs = xs |> Seq.sortBy (fun _ -> next())
        [1..100] |> shuffle (fun _ -> r.Next()) |> Seq.take 3

    // want a single random element
    let singleRandomElementFromList () = 
        let shuffleR (r : Random) xs = xs |> Seq.sortBy (fun _ -> r.Next())
        [1..100] |> shuffleR (Random ()) |> Seq.head;;