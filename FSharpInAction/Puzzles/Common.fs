namespace Puzzles
module Common = 
    open System
    open System.IO


    let workingDirectory = Environment.CurrentDirectory
    let projectFolder = Directory.GetParent(workingDirectory).Parent.Parent.FullName

    let readAllLines filePath = 
        File.ReadAllLines (Path.Combine(projectFolder, filePath))

