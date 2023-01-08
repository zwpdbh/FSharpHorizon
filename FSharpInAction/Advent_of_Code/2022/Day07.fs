namespace AdventOfCode2022

module Day07 = 
    open Expecto

    type File = {Name: string; Size: int}
    type Dir = {Name: string}


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

    module Token = 
        type Token =
            | Ls 
            | Cd of Dir 
            | File of File
            | Directory of Dir 

        let (|ParseLsToken|_|) (input: Token list) = 
            match input with 
            | head::_ when head = Ls ->
                Some Ls 
            | _ -> None

        let (|ParseDirToken|_|) (input: Token list) = 
            match input with 
            | head::_ ->
                match head with 
                | Cd dir -> Some dir 
                | _ -> None 
            | _ -> None

        let (|ParseFileToken|_|) (input: Token list) = 
            match input with 
            | head::_ ->
                match head with 
                | Token.File file -> Some file 
                | _ -> None 
            | _ -> None

        let (|ParseDirectoryToken|_|) (input: Token list) = 
            match input with 
            | head::_ ->
                match head with 
                | Directory directory -> Some directory 
                | _ -> None 
            | _ -> None

        // Like parse string into token 
        let parseTerminalOutputLine (line: string) = 
            match line with 
            | ParseCd dirName -> Cd {Name = dirName}
            | ParseLs _ -> Ls 
            | ParseDir dirName -> Directory {Name = dirName}
            | ParseFile (size, name) -> File ({Size = size; Name = name})
            | _ -> 
                failwith $"unknow terminal output line: {line}"
    
        let testParser = 
            testCase "test parse string to token"
            <| fun _ -> 
                Expect.equal (parseTerminalOutputLine "$ cd /") (Cd {Name = "/"}) "01"
                Expect.equal (parseTerminalOutputLine "$ ls") (Ls) "02"        
                Expect.throws (fun () -> parseTerminalOutputLine "$ dir a" |> ignore) "03.a"
                Expect.equal (parseTerminalOutputLine "dir a") (Directory {Name = "a"}) "03.b" 
                Expect.equal (parseTerminalOutputLine "8504156 c.dat") (File {Size = 8504156; Name = "c.dat"}) "04"


        let tokenListExample = 
            [
                Cd { Name = "/" }
                Ls
                Directory { Name = "a" }
                File { Name = "b.txt"; Size = 14848514 }
                File { Name = "c.dat"; Size = 8504156 }
                Directory { Name = "d" }
                Cd { Name = "a" }
                Ls
                Directory { Name = "e" }
                File { Name = "f"; Size = 29116 }
                File { Name = "g"; Size = 2557 };
                File { Name = "h.lst"; Size = 62596 }
                Cd { Name = "e" }
                Ls
                File { Name = "i"; Size = 584 }
                Cd { Name = ".." }
                Cd { Name = ".." }
                Cd { Name = "d" }
                Ls
                File { Name = "j"; Size = 4060174 }
                File { Name = "d.log"; Size = 8033020 }
                File { Name = "d.ext"; Size = 5626152 }
                File { Name = "k"; Size = 7214296 }
            ]

        let terminalOutput = 
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

        let testTerminalOutput = 
            testCase "test parse terminal output into token lists"
            <| fun _ ->
                let result = 
                    parseTerminalOutput terminalOutput
                    |> List.map (fun eachLine -> parseTerminalOutputLine eachLine)

                Expect.sequenceEqual tokenListExample result ""


    module FileSystem = 
        open Token
        type FileType = 
            | File of File
            | Directory of Dir 
        
        type Node =
            | One of FileType
            | Many of Node list


        type FileSystem = 
            //// Here the path is the extra information we need to carry in parallel with Empty
            //| EmptyFolder of Path: string list (I try to think in this way and it confused me for a while!)
            // Empty is really useful, it helps us to set the base condition
            | Empty of Path: string list
            // Use Node to represent files and sub-folders makes things simpler (but may not accurate enough)
            // For example, Files and SubFolders should not be both None.
            | Node of Path: string list * Files: File list option * SubFolders: FileSystem list option
            //| JustFolder of {|Path: string list|} 
            //| FolderWithFiles of {|Path: string list; Files: File list|}
            //| FolderWithFolders of {|Path: string list; SubFolders: FileSystem list|} 
            //| FolderWithFilesAndFolders of {|Path: string list; Files: File list; SubFolders: FileSystem list|}


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

    //let currentDirectory (token: Token) (dirPath: string list option) = 
    //    match dirPath with 
    //    | None -> 
    //        match token with 
    //        | Ls -> None 
    //        | Cd dir -> 
    //            Some [dir.Name]
    //        | File _ -> 
    //            None 
    //        | Directory _ -> 
    //            None 
    //    | Some dirPath -> 
    //        match token with 
    //        | Ls -> 
    //            Some dirPath
    //        | Cd dir -> 
    //            Some (dir.Name :: dirPath)
    //        | File _ -> 
    //            Some dirPath
    //        | Directory _ -> 
    //            Some dirPath 

        let rec getOneGroup acc tokens = 
            match acc, tokens with 
            | Cd _ :: _, x :: (Cd _ :: _ as tail) -> 
                match x with 
                | Cd _ -> 
                    getOneGroup (x::acc) tail 
                | Ls -> 
                    failwith "Find Ls appears before Cd"
                | _ -> 
                    x::acc |> List.rev 
            | _, x :: (Cd _ :: _) -> 
                x :: acc 
                |> List.rev 
            | _, x :: tail -> 
                getOneGroup (x::acc) tail
            | _, _ ->
                acc |> List.rev

        let (|GetOneGroup|_|) tokens = 
            match tokens with 
            | Cd _ :: _ -> 
                Some (getOneGroup [] tokens)
            | _ -> 
                None 

        //let tokenListExample = 
        //    [
        //        Cd { Name = "/" }
        //        Ls
        //        Directory { Name = "a" }
        //        File { Name = "b.txt"; Size = 14848514 }
        //        File { Name = "c.dat"; Size = 8504156 }
        //        Directory { Name = "d" }
        //        Cd { Name = "a" }
        //        Ls
        //        Directory { Name = "e" }
        //        File { Name = "f"; Size = 29116 }
        //        File { Name = "g"; Size = 2557 };
        //        File { Name = "h.lst"; Size = 62596 }
        //        Cd { Name = "e" }
        //        Ls
        //        File { Name = "i"; Size = 584 }
        //        Cd { Name = ".." }
        //        Cd { Name = ".." }
        //        Cd { Name = "d" }
        //        Ls
        //        File { Name = "j"; Size = 4060174 }
        //        File { Name = "d.log"; Size = 8033020 }
        //        File { Name = "d.ext"; Size = 5626152 }
        //        File { Name = "k"; Size = 7214296 }
        //    ]

        let getFileSystemComponents tokenGroup = 
            let rec helper currentPath fileList dirList tokenGroup = 
                match tokenGroup with 
                | Cd dir :: rest -> 
                    helper (dir.Name :: currentPath) fileList dirList rest
                | Ls :: rest -> 
                    helper currentPath fileList dirList rest 
                | Token.File f :: rest -> 
                    helper currentPath (f::fileList) dirList rest 
                | Token.Directory d :: rest -> 
                    helper currentPath fileList (d::dirList) rest 
                | _ -> 
                    currentPath, fileList, dirList 
            helper [] [] [] tokenGroup


        let demo () = 
            getOneGroup [] tokenListExample[2..10]
            |> getFileSystemComponents

        //let rec insert (tokenGroup: Token list) (fileSystem: FileSystem) = 
        //    let pathes, files, dirs = getFileSystemComponents tokenGroup

        //    match fileSystem with 
        //    | Empty currentPath ->             
        //        let subFolders = 
        //            dirs 
        //            |> List.map (fun x -> 
        //                Node((x.Name::pathes), None, None)
        //            )
        //        Node (pathes @ currentPath, Some files, Some subFolders)
        //    | Node (currPath, files, subFolders) -> 
        //        // Comparing with BST: In bst, we insert new value by comparing with with current node's value             
        //        match tokenGroup with 
        //        | 

                //let matchedOne = 
                //    subFolders
                //    |> List.choose (fun x -> 
                //        match x with 
                //        | EmptyFolder -> None 
                //        | Node (subFolderPath, _, subFolderDirs) -> 
                //            match subFolderPath = newNodePath with 
                //            | true -> Some x 
                //            | false -> None                            
                //    )
                //    |> List.tryHead

                //match matchedOne with 
                //| None -> failwith $"There is no matched sub-folder from existing file system for newNodePath: {newNodePath}, currPath: {currPath}"
                //| Some Node (subFolderPath, _, subFolderDirs) as someFolder-> 

    

        //let testBuildFileSystem = 
        //    testCase "test build FileSystem02"
        //    <| fun _ -> 
        //        let fileSystem = 
        //            Node(
        //                Path = ["/"],
        //                Files = Some [{Name = "b.txt"; Size = 14848514}; {Name = "c.dat"; Size = 8504156}],
        //                SubFolders = Some [
        //                    Node(
        //                        Path = ["a"; "/"],
        //                        Files = Some [
        //                            {Name = "f"; Size = 29116}
        //                            {Name = "g"; Size = 2557}
        //                            {Name = "h.lst"; Size = 62596}
        //                        ],
        //                        SubFolders = Some [
        //                            Node(
        //                                Path = ["e"; "a"; "/"],
        //                                Files = Some [{Name = "i"; Size = 584}],
        //                                SubFolders = None)
        //                        ]
        //                    )
        //                    Node(
        //                        Path = ["d"; "/"],
        //                        Files = Some [
        //                            {Name = "j"; Size = 4060174}
        //                            {Name = "d.log"; Size = 8033020}
        //                            {Name = "d.ext"; Size = 5626152}
        //                            {Name = "k"; Size = 7214296}
        //                        ],
        //                        SubFolders = None
        //                    )
        //                ]
        //            )
        //        Expect.isTrue true ""



    module Part01 = 
        let test01 = 
            testCase "part01 baseline"
            <| fun _ -> 
                Expect.isTrue true ""

    [<Tests>]
    let tests = testList "Day 07" [Token.testParser; Token.testTerminalOutput; Part01.test01]

