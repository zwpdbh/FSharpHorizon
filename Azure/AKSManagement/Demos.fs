namespace AKSManagement
module Demos = 
    let demoListSnapshot () = 
        AzureAPI.Snapshot.listAllSnapshots "33922553-c28a-4d50-ac93-a5c682692168"
        |> Async.RunSynchronously

