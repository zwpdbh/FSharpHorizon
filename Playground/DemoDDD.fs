namespace DDD 


module DemoDDD = 
    //Now ValidateAddress can’t be created from outside the containing module due to the private constructor
    type ValidateAddress = private ValidateAddress of string 

    //If we write code in the same module that contains the type definition above, then we can access the constructor.
    //Let’s use this fact to define some functions that will help us manipulate the
    //type. We’ll start by creating a submodule with exactly the same name
    module ValidateAddress = 
        let create (address: string) = 
            if address.Length <= 4 then 
                Error "Address must be at least 5 characters"
            else if address.Length > 30 then 
                Error "Address must be no longer than 30 characters"
            else 
                Ok (ValidateAddress address)

        let value (ValidateAddress address) = address 

    // p105 DDD
    let demo01 () = 
        let validAddressResult = ValidateAddress.create "some"
        match validAddressResult with 
        | Error msg -> 
            failwith msg 
        | Ok validAddress -> 
            let innerValue = ValidateAddress.value validAddress
            printfn "inner value is %A" innerValue