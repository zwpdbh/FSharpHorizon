namespace AdventOfCode2022

module Day07 = 
    open Expecto

    //type file = 
    //| File of {|Name: string; Size: int|}
    //| Directory of {|Name: string; Content: file list|}

    type File = {FileName: string; Size: int}
    type Dir = {DirectoryName: string}

    //type File = 
    //| File of FileName 
    //| Directory of Dir 

    type Output =
    | Ls 
    | Cd of Dir 
    | File of File
    | Directory of Dir 

    type FileSystemType = 
    | File of File 
    | Dir of Dir 

    type FileSystem = 
    | One of FileSystemType 
    | Many of FileSystem list 


    let parseTerminalOutput  (output: string) = 
        output.Split '\n'
        |> List.ofArray
        |> List.map (fun each -> each.Trim())
        |> List.filter (fun each -> each.Length > 0)

    
    let (|ParseCd|_|) (input: string) =
        let inputArray = input.Split " "
        try 
            if  inputArray.Length = 3  
                && inputArray[0] ="$" 
                && inputArray[1]  = "cd" 
                then 
                Some inputArray[2]
            else 
                None
        with _ ->
            None 

    let (|ParseLs|_|) (input: string) = 
        let inputArray = input.Split " "
        try 
            if  inputArray.Length = 2  
                && inputArray[0] ="$" 
                && inputArray[1]  = "ls" 
                then 
                Some ""
            else 
                None
        with _ ->
            None 

    let (|ParseDir|_|) (input: string) = 
        let inputArray = input.Split " "
        try 
            if  inputArray.Length = 2  
                && inputArray[0] ="dir" 
                then 
                Some inputArray[1]
            else 
                None
        with _ ->
            None 

    let (|ParseFile|_|) (input: string) = 
        let inputArray = input.Split " "
        try 
            if  inputArray.Length = 2  then
                Some (int inputArray[0], inputArray[1])
            else 
                None
        with _ ->
            None 

    // Like parse string into token 
    let parseTerminalOutputLine (line: string) = 
        match line with 
        | ParseCd dirName -> Cd {DirectoryName = dirName}
        | ParseLs _ -> Ls 
        | ParseDir dirName -> Directory {DirectoryName = dirName}
        | ParseFile (size, name) -> File ({Size = size; FileName = name})
        | _ -> 
            failwith $"unknow terminal output line: {line}"
    
    let testParser = 
        testCase "test parse string to token"
        <| fun _ -> 
            Expect.equal (parseTerminalOutputLine "$ cd /") (Cd {DirectoryName = "/"}) "01"
            Expect.equal (parseTerminalOutputLine "$ ls") (Ls) "02"        
            Expect.throws (fun () -> parseTerminalOutputLine "$ dir a" |> ignore) "03.a"
            Expect.equal (parseTerminalOutputLine "dir a") (Directory {DirectoryName = "a"}) "03.b" 
            Expect.equal (parseTerminalOutputLine "8504156 c.dat") (File {Size = 8504156; FileName = "c.dat"}) "04"

    let demo () = 
        let output = 
            """
            $ cd /
            $ ls
            dir a
            14848514 b.txt
            8504156 c.dat
            dir d
            $ cd a
            $ ls
            dir e
            29116 f
            2557 g
            62596 h.lst
            $ cd e
            $ ls
            584 i
            $ cd ..
            $ cd ..
            $ cd d
            $ ls
            4060174 j
            8033020 d.log
            5626152 d.ext
            7214296 k
            """
        parseTerminalOutput output
        |> List.map (fun eachLine -> parseTerminalOutputLine eachLine)



    module Part01 = 
        let test01 = 
            testCase "part01 baseline"
            <| fun _ -> 
                Expect.isTrue true ""

    [<Tests>]
    let tests = testList "Day 07" [testParser; Part01.test01]

