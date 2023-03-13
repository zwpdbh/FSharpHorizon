namespace FunctionalPatterns.ReactiveProgramming 

module PubSub = 
    module Example01 = 
        type MyEventArgs(x: int) =
            inherit System.EventArgs()
            member this.X = x
        
        type MyClass() =
            let myEvent = new Event<MyEventArgs>()
            member this.MyEvent = myEvent.Publish
            member this.Trigger(x) = myEvent.Trigger(MyEventArgs(x))
        
        let demo () = 
            let c = MyClass()
            c.MyEvent.Add(fun args -> printfn "MyEvent triggered with value %d" args.X)
            c.Trigger(3) // prints "MyEvent triggered with value 3"


    module Example02 = 
        // https://learn.microsoft.com/en-us/shows/rx-workshop/
        let demo () = 0
    

    // TBD: http://fsprojects.github.io/FSharp.Control.Reactive/tutorial.html
    // TBD: https://github.com/fsprojects/FSharp.Control.Reactive/blob/master/tests/ObservableSpecs.fs
    module Example03 = 
        open System
        open System.Reactive.Concurrency
        open System.Reactive.Disposables
        open System.Reactive.Linq
        open FSharp.Control.Reactive
        open FSharp.Control.Reactive.Builders
        open FSharp.Control.Reactive.Observable
        open System.Reactive.Subjects

        let demo () = 0


    // Compare Example04 and Example05
    module Example04 = 
        open System.IO 
        let fileWatcher = new FileSystemWatcher(@"C:\Test") 

        fileWatcher.EnableRaisingEvents <- true

        let isNotHidden(fse:RenamedEventArgs) = 
            let hidden = FileAttributes.Hidden 
            (File.GetAttributes(fse.FullPath) &&& hidden) <> hidden

        fileWatcher.Renamed.Add(fun fse -> 
            if isNotHidden(fse) then 
                printfn "%s renamed to %s" fse.OldFullPath fse.FullPath)


    module Example05 = 
        open System.IO 
        let fileWatcher = new FileSystemWatcher(@"C:\Test")

        let isNotHidden(fse:RenamedEventArgs) = 
            let hidden = FileAttributes.Hidden 
            (File.GetAttributes(fse.FullPath) &&& hidden) <> hidden

        let renamedVisible = 
            fileWatcher.Renamed |> Observable.filter isNotHidden

        renamedVisible |> Observable.add (fun fse -> 
            printfn "%s renamed to %s" fse.OldFullPath fse.FullPath)





