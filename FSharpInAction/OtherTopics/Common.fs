namespace OtherTopics

module Common = 
    open System
    open System.IO

    let workingDirectory = Environment.CurrentDirectory
    // We need to go to its "../../" because it is running from output folder
    let projectFolder = Directory.GetParent(workingDirectory).Parent.Parent.FullName

    let filePathInProject filePath = 
        Path.Combine(projectFolder, filePath)

    let readInput filePath = 
        File.ReadAllLines (filePathInProject filePath)

