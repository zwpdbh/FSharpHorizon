namespace DDD 


module DemoDDD = 
    type ValidateAddress = private ValidateAddress of string 

    module ValidateAddress = 
        let create (address: string) = 
            if address.Length <= 4 then 
                Error "Address must be at least 5 characters"
            else if address.Length > 30 then 
                Error "Address must be no longer than 30 characters"
            else 
                Ok (ValidateAddress address)

        let value (ValidateAddress address) = address 

    let demo() = 
        let validAddressResult = ValidateAddress.create "some"
        match validAddressResult with 
        | Error msg -> 
            failwith msg 
        | Ok validAddress -> 
            let innerValue = ValidateAddress.value validAddress
            printfn "inner value is %A" innerValue