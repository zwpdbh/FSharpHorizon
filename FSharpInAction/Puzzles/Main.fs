namespace Puzzles

module PuzzlesMain = 
    open Expecto

    [<EntryPoint>]
    let main argv =
        //Tests.runTestsInAssembly defaultConfig argv
        // BackTracking.EightQueens.demoAllNQueenSolutions()
        BackTracking.Subsets.demoSubsets ()
        0