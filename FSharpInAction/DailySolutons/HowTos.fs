namespace DailySolutions 


module HowTos = 
    

    let saveDailyWallpapers () = 
        let assetPath = @"C:\Users\wei\AppData\Local\Packages\Microsoft.Windows.ContentDeliveryManager_cw5n1h2txyewy\LocalState\Assets"
        let todayStr = System.DateTime.Today.ToString("yyyy-MM-dd")
        let destPath = System.IO.Path.Combine(@"E:\Download", "wallpaper-" + todayStr)

        if System.IO.Directory.Exists(destPath) then 
            System.IO.Directory.Delete(destPath, true)
         
        System.IO.Directory.CreateDirectory(destPath) |> ignore

        System.IO.Directory.GetFiles(assetPath)
        |> Array.map (fun each -> 
            let fileName = each.Split("\\") |> Array.last
            each, System.IO.Path.Combine(destPath, fileName + ".jpg")
        )
        |> Array.iter (fun (srcFile, destFile) -> 
            System.IO.File.Copy(srcFile, destFile)
            //printfn $"{srcFile} -- {destFile}"
        )

        printfn "Done"


    let renameFilesInFolder () = 
        let assetPath = @"E:\Download\wallpapers-0202"
        System.IO.Directory.GetFiles assetPath
        |> Array.iter (fun each -> 
            System.IO.File.Move(each, each + ".jpg")
        )
