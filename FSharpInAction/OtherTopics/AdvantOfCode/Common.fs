namespace Others.AdventOfCode

module Common = 
    open System
    open System.IO

    let workingDirectory = Environment.CurrentDirectory
    // We need to go to its "../../" because it is running from output folder
    let projectFolder = Directory.GetParent(workingDirectory).Parent.Parent.FullName

    let readInput filePath = 
        File.ReadAllLines (Path.Combine(projectFolder, filePath))