namespace Others.FromInterview

open Others
open Expecto

module Tmp = 

    module Problem01 = 

        let demo() = 
            0


        let test01 = 
            testCase "test01"
            <| fun _ ->
                Expect.isTrue true "Problem01 test01"

    [<Tests>]
    let tests = testList "FromInterview.Tmp" [Problem01.test01]