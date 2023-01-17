namespace Puzzles
open Expecto
open System
open System.IO

module Caddi =
    module Problem01 = 
        let test01 = 
            testCase "Problem01.1"
            <| fun _ -> 
                Expect.isTrue true ""


    [<Tests>]
    let tests = testList "Ciddi Problems" [
        Problem01.test01
    ]
