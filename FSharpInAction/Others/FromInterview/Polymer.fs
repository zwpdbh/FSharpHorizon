namespace Others.FromInterview

open Others
open Expecto

module Polymer = 
    module Problem01 = 
        let test00 = 
            testCase "test00"
            <| fun _ -> 
                Expect.isTrue true "00"

        let demo () = 
            0
    [<Tests>]
    let tests = testList "FromInterview.Polymer" [Problem01.test00] 



