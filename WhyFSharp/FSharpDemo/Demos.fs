namespace WhyFSharp

module Demo01 =
    // Eliminate duplicates
    // Shows pattern matching is powerful
    // Shows it is generic by default
    // SHows it doesn't accept null
    let rec compress =
        function
        | a :: (b :: _ as tail) ->
            // Instead of keep the first and ignore following repeated as I first though
            // It continue to see next until meet different element to keep the last one !
            if a = b then
                compress tail
            else
                a :: (compress tail)
        | x -> x

    let demo01 () =
        compress [ "a"
                   "a"
                   "a"
                   "a"
                   "b"
                   "c"
                   "c"
                   "a"
                   "a"
                   "d"
                   "e"
                   "e"
                   "e"
                   "e" ]

    let demo02 () = 
        compress [1; 1; 2; 2; 3; 3; 3; 3; 4]
