namespace XscnScenario

module Main = 
    [<EntryPoint>]
    let main argv = 

        //ScenarioVmDeployment.demoListVirtualMachineDeployments () 
        ScteTools.checkBlobExportedBySCTE () |> printfn "%A"
        //ScteTools.getAccountKeyDemo () |> printfn "%A"
        0


