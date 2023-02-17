namespace Others.FromInterview

open Others
open Expecto

module Polymer = 


    let test00 = 
        testCase "test00"
        <| fun _ -> 
            Expect.isTrue true "00"

    [<Tests>]
    let tests = testList "FromInterview.Polymer" [test00] 



