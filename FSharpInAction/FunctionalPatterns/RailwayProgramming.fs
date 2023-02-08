namespace FunctionalPatterns

module RailwayProgramming = 
    module FizzBuzzMatch = 
        let fizzBuzz i = 
            match i with
            | _ when i % 15 = 0 -> 
                printf "FizzBuzz"
            | _ when i % 3 = 0 -> 
                printf "Fizz"
            | _ when i % 5 = 0 -> 
                printf "Buzz"
            | _ -> 
                printf "%i" i

            printf "; "

        // do the fizzbuzz
        [1..100] |> List.iter fizzBuzz    


    module FizzBuzzIfPrime = 
        let fizzBuzz i = 
            let mutable printed = false

            if i % 3 = 0 then
                printed <- true
                printf "Fizz"

            if i % 5 = 0 then
                printed <- true
                printf "Buzz"

            if not printed then
                printf "%i" i

            printf "; "

        // do the fizzbuzz
        [1..100] |> List.iter fizzBuzz

    module FizzBuzzUsingFactorRules = 
        let fizzBuzz rules i  = 
            let mutable printed = false

            for factor,label in rules do
                if i % factor = 0 then
                    printed <- true
                    printf "%s" label

            if not printed then
                printf "%i" i

            printf "; "

        // do the fizzbuzz
        let rules = [ (3,"Fizz"); (5,"Buzz") ]
        [1..100] |> List.iter (fizzBuzz rules)

        let rules2 = [ (3,"Fizz"); (5,"Buzz"); (7,"Baz") ]
        [1..105] |> List.iter (fizzBuzz rules2)


    module FizzBuzzUsingPipelineV1 = 
        type Data = 
            {
                i: int 
                label: string option
            }

        let carbonate factor label data = 
            let {i = i; label= labelSoFar } = data 

            if i % factor = 0 then 
                let newLabel = 
                    match labelSoFar with 
                    | Some s -> s + label
                    | None -> label

                {data with label = Some newLabel}
            else 
                data 

        let labelForDefault data = 
            let {i = i; label = labelSoFar} = data 

            match labelSoFar with 
            | None -> $"{i}"
            | Some s -> s

        let fizzBuzz i = 
            {i = i; label = None}
            |> carbonate 3 "Fizz"
            |> carbonate 5 "Buzz"
            |> labelForDefault 
            |> printfn "%s"

        let demo () = 
            [1..100] |> List.iter fizzBuzz

    module FizzBuzzUsingPipelineV2 = 
        type Data = int * string option

        let carbonate factor label data = 
            let (i,labelSoFar) = data
            if i % factor = 0 then
                
                let newLabel = 
                    labelSoFar 
                    |> Option.map (fun s -> s + label)
                    |> defaultArg <| label 
                (i,Some newLabel)
            else
                
                data

        let labelOrDefault data = 
            let (i,labelSoFar) = data
            labelSoFar 
            |> defaultArg <| sprintf "%i" i

        let fizzBuzz i = 
            (i,None)   
            |> carbonate 3 "Fizz"
            |> carbonate 5 "Buzz"
            |> labelOrDefault     
            |> printfn "%s"      
           
        let demo () = 
            [1..100] |> List.iter fizzBuzz