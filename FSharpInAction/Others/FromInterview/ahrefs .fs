namespace Others.FromInterview


open Expecto
module Ahrefs = 

    module Problem01 = 
        let test00 = 
            testCase "test00"
            <| fun _ -> 
                Expect.isTrue true "00"

    [<Tests>]
    let tests = testList "FromInterview.Ahrefs" [Problem01.test00] 