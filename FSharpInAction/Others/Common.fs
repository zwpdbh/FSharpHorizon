﻿namespace Others

module Common = 
    open System
    open System.IO

    let workingDirectory = Environment.CurrentDirectory
    // We need to go to its "../../" because it is running from output folder
    let projectFolder = Directory.GetParent(workingDirectory).Parent.Parent.FullName

    let getPath filePathInProject = 
        Path.Combine(projectFolder, filePathInProject)

    let readLines filePath = 
        File.ReadAllLines (getPath filePath)

    let readText filePath = 
        File.ReadAllText (getPath filePath)
