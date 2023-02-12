namespace FunctionalPatterns

/// From: https://swlaschin.gitbooks.io/fsharpforfunandprofit/content/posts/railway-oriented-programming-carbonated.html
/// Also it links from https://fsharpforfunandprofit.com/posts/recipe-part1/
module RailwayProgramming = 
    module FizzBuzzMatch = 
        // Downside is: 
        // 1) The rules are hard coded.
        // 2) The order of rules are important
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
        // The order of rules are not important. but the rules are still hardcode.
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
        // Rules are factor out and the order of the are not important.
        // How can we remove the mutable?
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
        // We want to piple through rules to get result without mutable variable.
        // This version is not elegent.
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

        let labelOrDefault data = 
            let {i = i; label = labelSoFar} = data 

            match labelSoFar with 
            | None -> $"{i}"
            | Some s -> s

        let fizzBuzz i = 
            {i = i; label = None}
            |> carbonate 3 "Fizz"
            |> carbonate 5 "Buzz"
            |> labelOrDefault 
            |> printfn "%s"

        let demo () = 
            [1..100] |> List.iter fizzBuzz

    module FizzBuzzUsingPipelineV2 = 
        // Things havelearned:
        // defaultArg
        //> defaultArg (Some 100) -1;;
        //> val it: int = 100

        //> defaultArg None -1;;
        //val it: int = -1

        // Comparing with V1, we simply use tuple to replace complex custom type.
        type Data = int * string option

        let carbonate factor label data = 
            let (i,labelSoFar) = data
            if i % factor = 0 then
                
                let newLabel = 
                    labelSoFar 
                    |> Option.map (fun s -> s + label)
                    // The pipe-backward operator takes a function on the left and applies it to a value on the right.
                    // Something like allowing you to change operator precedence.
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


    module FizzBuzzUsingPipelineV3 = 
        // Comparing with previous one, this one:
        // Combine multiple rules into one rule function, using reduce.
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

        let rules = [ (3,"Fizz"); (5,"Buzz"); (7,"Baz") ]

        // Notice the partial function in lambda function.
        let allRules = 
            rules 
            |> List.map (fun (factor, label) -> carbonate factor label)
            |> List.reduce (>>)

        let labelOrDefault data = 
            let (i,labelSoFar) = data
            labelSoFar 
            |> defaultArg <| sprintf "%i" i

        let fizzBuzz i = 
            (i,None)   
            |> allRules
            |> labelOrDefault     
            |> printfn "%s"      
           
        let demo () = 
            [1..100] |> List.iter fizzBuzz


    module RailwayCombinatorModule = 
        // A common moduel which will be used across following modules
        // Previous one is the pipeline version.
        // Here, we begin new explore, using rail programming pattern.

        // Convert a single value into a two-track result 
        let succeed x = Result.Ok x 

        // Convert a single value into a two-track result
        let fail x = Result.Error x 

        // appy either a success function or failure function
        let either successFunc failureFunc twoTrackInput =
            match twoTrackInput with
            | Ok s -> successFunc s
            | Error f -> failureFunc f

        // convert a switch function into a two-track function
        let bind f = 
            either f fail        


    module FizzBuzz_RailwayOriented_CarbonationIsSuccess =  
        open RailwayCombinatorModule

        // the Success track contains the labels, and the Failure track contains the ints.
        let carbonate factor label i = 
            if i % factor = 0 then 
                Ok label 
            else 
                Error i 

        // Need to connect the components together
        // Logic is: 
        // if the int is already carbonated, ignore it; 
        // if the int is not carbonated, connect it to the input of the next switch function
        let connect f = 
            function
            | Ok x -> succeed x 
            | Error i -> f i 

        // Another way of writing "connect" is to use the either function we defined in the library
        let connectv2 f = 
            either succeed f 

        // The switches are connected together through composition (>>) rather than piping (|>).
        let fizzBuzz = 
            carbonate 15 "FizzBuzz"
            >> connect (carbonate 3 "Fizz")
            >> connect (carbonate 5 "Buzz")
            >> either (printf "%s") (printf "%i")
        
        let demo () = 
            [1..100] |> List.iter fizzBuzz


    module FizzBuzz_RailwayOriented_CarbonationIsFailure = 

        open RailwayCombinatorModule 

        // carbonate a value
        let carbonate factor label i = 
            if i % factor = 0 then
                fail label
            else
                succeed i

        let fizzBuzz = 
            carbonate 15 "FizzBuzz"
            >> bind (carbonate 3 "Fizz")
            >> bind (carbonate 5 "Buzz")
            >> either (printf "%i; ") (printf "%s; ") 

        let demo () = 
            [1..100] |> List.iter fizzBuzz


    module FizzBuzz_RailwayOriented_UsingCustomChoice = 
        
        open RailwayCombinatorModule

        // why don't we keep the two-track idea, but get rid of the "Success" and "Failure" labels.
        // Instead, we can call one track "Carbonated" and the other "Uncarbonated".
        // Use an active pattern to map raw data into domain data
        let (|Uncarbonated|Carbonated|) x =
            match x with 
            | Ok u -> Uncarbonated u
            | Error c -> Carbonated c

        /// convert a single value into a two-track result !!!
        let uncarbonated x = Ok x
        let carbonated x = Error x

        // carbonate a value
        let carbonate factor label i = 
            if i % factor = 0 then
                carbonated label
            else
                uncarbonated i

        let connect f x = 
            match x with
            | Uncarbonated i -> f i
            | Carbonated x -> carbonated x 

        let connect' f = 
            either f carbonated 

        let fizzBuzz = 
            carbonate 15 "FizzBuzz"
            >> connect (carbonate 3 "Fizz")
            >> connect (carbonate 5 "Buzz")
            >> either (printf "%i; ") (printf "%s; ") 

        // test
        [1..100] |> List.iter fizzBuzz

        // Things need to get used to: use >>; when we could use |> .
        let understandFunctionCompositionOperator () = 
            let negate x = x * -1 
            let square x = x * x 
            let print  x = printfn "The number is: %d" x
            let square_negate_then_print = square >> negate >> print 

            square_negate_then_print 10

    module FizzBuzz_RailwayOriented_UsingAppend = 
        open RailwayCombinatorModule

        let (|Uncarbonated|Carbonated|) x =
            match x with 
            | Ok u -> Uncarbonated u
            | Error c -> Carbonated c

        /// convert a single value into a two-track result !!!
        let uncarbonated x = Ok x
        let carbonated x = Error x

        // Instead of combining all the "switch" functions in series, we can "add" them together in parallel.
        // Use it for doing all the factors at once.
        // The trick is to define a "append" or "concat" function for combining two functions
        let (<+>) switch1 switch2 x = 
            match (switch1 x), (switch2 x) with 
            | Carbonated s1, Carbonated s2 -> carbonated (s1 + s2)
            | Uncarbonated f1,Carbonated s2  -> carbonated s2
            | Carbonated s1,Uncarbonated f2 -> carbonated s1
            | Uncarbonated f1,Uncarbonated f2 -> uncarbonated f1

        // carbonate a value
        let carbonate factor label i = 
            if i % factor = 0 then
                carbonated label
            else
                uncarbonated i

        let fizzBuzz = 
            let carbonateAll = 
                carbonate 3 "Fizz" <+> carbonate 5 "Buzz"

            carbonateAll 
            >> either (printf "%i; ") (printf "%s; ")

        let demo () = 
            [1..100] |> List.iter fizzBuzz

    /// Same as "FizzBuzz_RailwayOriented_UsingAppend" but using reduce to all all the rules
    module FizzBuzz_RailwayOriented_UsingAddition = 
        
        open RailwayCombinatorModule

        /// convert a single value into a two-track result !!!
        let uncarbonated x = Ok x
        let carbonated x = Error x

        let (|Uncarbonated|Carbonated|) x =
            match x with 
            | Ok u -> Uncarbonated u
            | Error c -> Carbonated c

        // Instead of combining all the "switch" functions in series, we can "add" them together in parallel.
        // Use it for doing all the factors at once.
        // The trick is to define a "append" or "concat" function for combining two functions
        let (<+>) switch1 switch2 x = 
            match (switch1 x), (switch2 x) with 
            | Carbonated s1, Carbonated s2 -> carbonated (s1 + s2)
            | Uncarbonated f1,Carbonated s2  -> carbonated s2
            | Carbonated s1,Uncarbonated f2 -> carbonated s1
            | Uncarbonated f1,Uncarbonated f2 -> uncarbonated f1

        // carbonate a value
        let carbonate factor label i = 
            if i % factor = 0 then
                carbonated label
            else
                uncarbonated i

        let fizzBuzzPrimes rules = 
            let carbonateAll  = 
                rules
                |> List.map (fun (factor,label) -> carbonate factor label)
                |> List.reduce (<+>)

            carbonateAll 
            >> either (printf "%i; ") (printf "%s; ") 

        let rules = [ (3,"Fizz"); (5,"Buzz"); (7,"Baz") ]

        let demo () = 
            [1..100] |> List.iter (fizzBuzzPrimes rules)        