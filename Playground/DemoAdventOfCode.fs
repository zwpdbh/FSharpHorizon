namespace DemoAdventOfCode2022
module Day07 = 
    open AdventOfCode2022.Day07
    open AdventOfCode2022.Day07.Token
    open AdventOfCode2022.Day07.FileSystem

    let tokens = 
        parseTerminalOutput terminalOutput
        |> List.map (fun eachLine -> parseTerminalOutputLine eachLine)

    let build (tokens: Token list) = 
        let rec helper (tokens: Token list) (node: Node)= 
            match tokens with 
            | GetOneGroup (tokenGroup, rest) -> 
                helper rest (insert tokenGroup node)
            | _ -> 
                printfn "could not find next valid group of tokens"
                node 
        helper tokens Empty 

    tokens |> build |> printfn "%A"


