namespace WhyFSharp

module Demo01 =
    // Eliminate duplicates
    // Shows pattern matching is powerful
    // Shows it is generic by default
    // Shows it doesn't accept null
    // Shows it prefer recursive over loop 
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
        compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]

    let demo02 () = 
        compress [1; 1; 2; 2; 3; 3; 3; 3; 4]


module Demo02 = 
    // Shows the pipeline operator
    // Shows the lambda 
    // Shows again loop like for i = 0; i <= 100; i++ is barely used in F#
    // The point is F# (FP) focus on the transformation of input to output
    let demo() = 
        let fizzBuzz x = 
            match x with 
            | _ when x % 3 = 0 && x % 5 = 0 -> 
                "FizzBuzz"
            | _ when x % 3 = 0 -> 
                "Fizz"
            | _ when x % 5 = 0 -> 
                "Buzz"
            | _ ->
                $"{x}"

        [1..100]
        |> List.iter (fun x -> fizzBuzz x |> printfn "%A")


module Demo03 = 
    // Shows DU used as inheritance. Inheritance usually can be considered as composition
    // Shows explict convert from int to float
    // Show they are just functions
    type Point = {x: int; y: int}

    type Shape = 
        | Rectangle of width: int * height: int * point: Point
        | Square of edge: int * point: Point
        | Circle of radius: int * point: Point

    let rectangle = Rectangle (20, 10, {x = 1; y = 2})
    let squre = Square (10, {x = 0; y = 0})
    let circle = Circle (10, {x = 0; y = 2})

    let area shape = 
        match shape with 
        | Rectangle (x, y, _) -> float (x * y )
        | Square (e, _) -> float (e * e)
        | Circle (r, _) -> 3.14 * float r * float r 

    let demo() = 
        [rectangle; squre; circle]
        |> List.map (fun x -> area x)
        |> List.sum