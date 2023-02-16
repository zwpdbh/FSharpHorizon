namespace Others.AdventOfCode.Code2022

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
        
        // v1
        type FileSystem = 
            | Empty of Path: string list
            | Node of Path: string list * Files: File list option * SubFolders: FileSystem list option

        // v2 vs v1 which one is better?
        type Node =
            | Empty
            | File of File
            | Directory of string * Node list 

        // v2 is better because from the below v1 and v2 instance show that only one DU case from v1 is used. 
        // It should be a signal which the DU is badly designed.
        // In v2, the case of DU is used properly to represent directories and files
        let demoAST01 () = 
            Node(
                ["/"],
                Some [{Name = "b.txt"; Size = 14848514}; {Name = "c.dat"; Size = 8504156}],
                Some [
                    Node(
                        ["a"; "/"],
                        Some [
                            {Name = "f"; Size = 29116}
                            {Name = "g"; Size = 2557}
                            {Name = "h.lst"; Size = 62596}
                        ],
                        Some [
                            Node(
                                ["e"; "a"; "/"],
                                Some [{Name = "i"; Size = 584}],
                                None)
                        ]
                    )
                    Node(
                        ["d"; "/"],
                        Some [
                            {Name = "j"; Size = 4060174}
                            {Name = "d.log"; Size = 8033020}
                            {Name = "d.ext"; Size = 5626152}
                            {Name = "k"; Size = 7214296}
                        ],
                        None
                    )
                ]
            )

        let demoAST02 () = 
            Directory("/", [
                Directory("a", [
                    Directory("e", [])
                    File({Name="f"; Size=29116})
                    File({Name="g"; Size=2557})
                    File({Name="h.lst"; Size = 62596})
                ])
                File({Name="b.txt"; Size = 14848514})
                File({Name="c.dat"; Size = 8504156})
                Directory("d", [
                    File({Name="j"; Size = 4060174})
                    File({Name="d.log"; Size = 8033020})
                    File({Name="d.ext"; Size = 5626152})
                    File({Name="k"; Size = 7214296})
                ])
            ])


        // A helper function which extract a group of tokens which belongs to some output.
        // Those tokens shows based on some directory, to see some content below some path, all the operations needed.
        let (|GetOneGroup|_|) tokens = 
            let rec getOneGroup acc tokens = 
                match acc, tokens with 
                | Cd _ :: _, x :: (Cd _ :: _ as tail) -> 
                    match x with 
                    | Cd _ -> 
                        getOneGroup (x::acc) tail 
                    | Ls -> 
                        failwith "Find Ls appears before Cd"
                    | _ -> 
                        x::acc |> List.rev, tail 
                | _, x :: (Cd _ :: _ as tail) ->       
                     (x :: acc)|> List.rev, tail
                | _, x :: tail -> 
                    getOneGroup (x::acc) tail
                | _, rest ->
                    acc |> List.rev, rest
                  
            match tokens with 
            | Cd _ :: _ -> 
                Some (getOneGroup [] tokens)
            | _ -> 
                None 


        // A tokenGroup is a list of tokens which contains Cd, Ls until next Cd, Ls 
        let rec insert (tokenGroup: Token list) (node: Node) = 
            let update token node = 
                match token with 
                | Ls -> node 
                | Cd dir -> 
                    match node with 
                    | Empty -> 
                        Directory (dir.Name, [])
                    | File x -> 
                        failwith $"meet Cd {dir} while visiting File {x}"
                    | Directory (currPath, nodes) -> 
                        match nodes with 
                        | (Directory (subFolder, _) as x)::_ when subFolder = dir.Name ->
                            x 
                        | [] -> 
                            Directory (dir.Name, [])
                        | _ -> 
                            failwith $"there is not way to Cd {dir.Name} from {currPath}"
                | Token.File someFile -> 
                    match node with 
                    | Empty -> failwith "file has to be inside some directory"
                    | File _ -> failwith "could not create a file while visiting a file"
                    | Directory (currtPath, nodes) as currtDirectory-> 
                        let sameFile = 
                            nodes 
                            |> List.choose (fun x -> 
                                match x with 
                                | Empty -> None 
                                | Directory _ -> None 
                                | File existingFile -> 
                                    if existingFile = someFile then 
                                        Some (File existingFile)
                                    else 
                                        None
                            )
                            |> List.tryHead

                        match sameFile with 
                        | Some _ -> currtDirectory
                        | None -> 
                            // add new file 
                            Directory (currtPath, (File someFile)::nodes)
                | Token.Directory someDir -> 
                    match node with 
                    | Directory (currentPath, nodes) as currentDirectory ->
                        let sameDir = 
                            nodes 
                            |> List.choose (fun x -> 
                                match x with                                
                                | Directory (existingDirName, _) as existingDir-> 
                                    if existingDirName = someDir.Name then 
                                        Some existingDir
                                    else 
                                        None 
                                | _ -> None
                            )
                            |> List.tryHead

                        match sameDir with 
                        | Some _ -> 
                            currentDirectory
                        | None -> 
                            // add new dir 
                            Directory (currentPath, (Directory (someDir.Name, [])::nodes))
                    | _ -> 
                        failwith $"no meaning when token is {someDir}, while visiting visiting non-directory node"       

            match tokenGroup with 
            | x::tail -> 
                insert tail (update x node)
            | _ -> 
                node 



    module Part01 = 
        let test01 = 
            testCase "part01 baseline"
            <| fun _ -> 
                Expect.isTrue true ""

    [<Tests>]
    let tests = testList "AdventOfCode.Code2022.Day07" [Token.testParser; Token.testTerminalOutput; Part01.test01]

