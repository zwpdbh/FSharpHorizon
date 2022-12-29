namespace AdventOfCode2022

module Day07 = 
    open Expecto

    type File = {Name: string; Size: int}
    type Dir = {Name: string}

    type Token =
    | Ls 
    | Cd of Dir 
    | File of File
    | Directory of Dir 


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
        | ParseCd dirName -> Cd {Name = dirName}
        | ParseLs _ -> Ls 
        | ParseDir dirName -> Directory {Name = dirName}
        | ParseFile (size, name) -> Token.File ({Size = size; Name = name})
        | _ -> 
            failwith $"unknow terminal output line: {line}"
    
    let testParser = 
        testCase "test parse string to token"
        <| fun _ -> 
            Expect.equal (parseTerminalOutputLine "$ cd /") (Cd {Name = "/"}) "01"
            Expect.equal (parseTerminalOutputLine "$ ls") (Ls) "02"        
            Expect.throws (fun () -> parseTerminalOutputLine "$ dir a" |> ignore) "03.a"
            Expect.equal (parseTerminalOutputLine "dir a") (Directory {Name = "a"}) "03.b" 
            Expect.equal (parseTerminalOutputLine "8504156 c.dat") (Token.File {Size = 8504156; Name = "c.dat"}) "04"


    type Directory = 
    | EmptyFolder of {|Name: string|} 
    | FolderWithFiles of {|Name: string; Files: File list|}
    | FolderWithFolders of {|Name: string; SubFolders: Directory list|} 
    | FolderWithFilesAndFolders of {|Name: string; Files: File list; SubFolders: Directory list|}


    let ASTLookLike = 
        """
    - / (dir)
      - a (dir)
        - e (dir)
          - i (file, size=584)
        - f (file, size=29116)
        - g (file, size=2557)
        - h.lst (file, size=62596)
      - b.txt (file, size=14848514)
      - c.dat (file, size=8504156)
      - d (dir)
        - j (file, size=4060174)
        - d.log (file, size=8033020)
        - d.ext (file, size=5626152)
        - k (file, size=7214296)
        """

    //let buildFileSystem (input: Token list) = 
    //    let helper (x: Token) (y: Token) = 
            
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

    let testBuildFileSystem = 
        testCase "test build FileSystem02"
        <| fun _ -> 
            let fileSystem = 
                FolderWithFilesAndFolders {|
                    Name = "/"
                    Files = [{Name = "b.txt"; Size = 14848514}; {Name = "c.dat"; Size = 8504156}]
                    SubFolders = [
                        FolderWithFilesAndFolders {|
                            Name = "a"
                            Files = [
                                {Name = "f"; Size = 29116}
                                {Name = "g"; Size = 2557}
                                {Name = "h.lst"; Size = 62596}
                            ]
                            SubFolders = [
                                FolderWithFiles {|Name = "e"; Files = [{Name = "i"; Size = 584}]|}
                            ]
                        |}
                        FolderWithFilesAndFolders {|
                            Name = "d"
                            Files = []
                            SubFolders = [
                                FolderWithFiles {|
                                    Name = "d"
                                    Files = [
                                        {Name = "j"; Size = 4060174}
                                        {Name = "d.log"; Size = 8033020}
                                        {Name = "d.ext"; Size = 5626152}
                                        {Name = "k"; Size = 7214296}
                                    ]
                                |}
                            ]
                        |}
                    ]
                |}
            Expect.isTrue true ""
        



    module Part01 = 
        let test01 = 
            testCase "part01 baseline"
            <| fun _ -> 
                Expect.isTrue true ""

    [<Tests>]
    let tests = testList "Day 07" [testParser; Part01.test01]

