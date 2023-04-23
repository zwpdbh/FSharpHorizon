namespace Others.Snippets 

module DuckTyping =
    // Ref: https://www.compositional-it.com/news-blog/static-duck-typing-in-f/
    // What if you could write a function that can take any type that you pass in as long as it has the property .Length, returning an int
    
    // Check: statically resolved type parameters (SRTPs).
    let inline Length x = (^a : (member Length : _) x)
    let demoLength () = 

        Length "text" |> printfn "%A"
        Length [ 1; 2; 3 ] |> printfn "%A"


    // Notice: We don't need to add any SRTP syntax explicitly, but the compiler will infer all of the SRTP constraints automatically.
    let inline describeLength x =
        $"This has a length of %i{Length x}"
    
    let inline Head x = (^a : (member Head : _) x)

    // We could combine them
    let inline HeadLength x = x |> Head |> Length

    let demoHeadLength () = 
        HeadLength [ [ 1 ] ] |> printfn "%A"
        HeadLength [ "text" ] |> printfn "%A"