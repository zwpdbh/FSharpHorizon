namespace FunctionalPatterns.ApplicativeParsers

module ParserFoundations =
    // From Series "Understanding Parser Combinators"
    // https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/
    open System

    module ParsingACharV1 =
        //The input to a parser is a stream of characters. We could use something complicated, but for now we’ll just use a string.
        //If the stream is empty, then return a pair consisting of false and an empty string.
        //If the first character in the stream is an A, then return a pair consisting of true and the remaining stream of characters.
        //If the first character in the stream is not an A, then return false and the (unchanged) original stream of characters.
        
        // Don't do this ParseResult<'a> = Result<'a, string>
        type ParseResult<'a> = 
            | Ok of 'a 
            | Error of string

        let pchar charToMatch str =
            if String.IsNullOrEmpty(str) then
                Error "No more input"
            else
                let first = str.[0]

                if first = charToMatch then
                    let remaining = str.[1..]
                    Ok (charToMatch, remaining)
                else
                    let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                    Error msg 

        let test = 
            let parseA = pchar 'A'
            // val parseA : string -> ParseResult<char * string> is bit of complicated, how to solve this? 
            // Let's encapsulate it in a "wrapper"
            parseA "ZBC" 

    module ParsingACharV2 = 
        // Don't do this ParseResult<'a> = Result<'a, string>
        type ParseResult<'a> = 
            | Ok of 'a 
            | Error of string

        // The benifit of encapsulating a function with type!
        // 1. It’s always good practice to use types to model the domain, and in this domain we are dealing with “parsers” not functions.
        // 2. It makes the type inference easier and helps to make the parser combinators
        // 3. Finally, it supports information hiding (via an abstract data type), so that we can later add metadata.
        // Step 1: Encapsulate "string -> ParseResult<char * string>" as Parser<char>.
        type Parser<'T> = Parser of (string -> ParseResult<'T * string>)


        let pchar charToMatch =
            // Step 2.1: replace the two parameters currying with an equivalent one-parameter function that returns an inner function.
            let innerFn str = 
                if String.IsNullOrEmpty(str) then
                    Error "No more input"
                else
                    let first = str.[0]

                    if first = charToMatch then
                        let remaining = str.[1..]
                        Ok (charToMatch, remaining)
                    else
                        let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                        Error msg 
            // Step 2.2: Don't forget to wrap the inner function with encapsulating type
            Parser innerFn

        // Step 3: Define a helper function which extracts the inner function and run it against the input stream
        let run parser input = 
            let (Parser innerFn) = parser 
            innerFn input

        let test = 
            let parseA = pchar 'A'
            // val parseA : string -> ParseResult<char * string> is bit of complicated, how to solve this? 
            // Let's encapsulate it in a "wrapper"
            run parseA "ZBC"  


    module ParsingAnd = 
        type ParseResult<'a> = 
            | Ok of 'a 
            | Error of string
        type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

        let pchar charToMatch =
            let innerFn str = 
                if String.IsNullOrEmpty(str) then
                    Error "No more input"
                else
                    let first = str.[0]

                    if first = charToMatch then
                        let remaining = str.[1..]
                        Ok (charToMatch, remaining)
                    else
                        let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                        Error msg 
            Parser innerFn

        let run parser input = 
            let (Parser innerFn) = parser 
            innerFn input

        let andThen parser1 parser2 = 
            let innerFn input = 
                let result1 = run parser1 input 

                match result1 with 
                | Error err-> 
                    Error err
                | Ok (value1, remaining1) ->
                    let result2 = run parser2 remaining1
                    match result2 with 
                    | Error err ->
                        Error err 
                    | Ok (value2, remaining2) -> 
                        let newValue = (value1, value2)
                        Ok (newValue, remaining2)

            Parser innerFn

        let (.>>.) = andThen 

        let demo () = 
            let parseA = pchar 'A'
            let parseB = pchar 'B'

            let parseAThenB = parseA .>>. parseB

            run parseAThenB "ABC" |> printfn "%A"


    module ParsingOr = 
        type ParseResult<'a> = 
            | Ok of 'a 
            | Error of string
        type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

        let pchar charToMatch =
            let innerFn str = 
                if String.IsNullOrEmpty(str) then
                    Error "No more input"
                else
                    let first = str.[0]

                    if first = charToMatch then
                        let remaining = str.[1..]
                        Ok (charToMatch, remaining)
                    else
                        let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                        Error msg 
            Parser innerFn

        let run parser input = 
            let (Parser innerFn) = parser 
            innerFn input

        let orElse parser1 parser2 = 
            let innerFn input = 
                let result1 = run parser1 input 
                match result1 with 
                | Ok _ -> 
                    result1 
                | Error _ -> 
                    let result2 = run parser2 input 
                    result2

            Parser innerFn

        let (<|>) = orElse 

        let demo () = 
            let parseA = pchar 'A'
            let parseB = pchar 'B'

            let parseAThenB = parseA <|> parseB
            run parseAThenB "BAB" |> printfn "%A"


    module ParseComplex = 
        type ParseResult<'a> = 
            | Ok of 'a 
            | Error of string
        type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

        let pchar charToMatch =
            let innerFn str = 
                if String.IsNullOrEmpty(str) then
                    Error "No more input"
                else
                    let first = str.[0]

                    if first = charToMatch then
                        let remaining = str.[1..]
                        Ok (charToMatch, remaining)
                    else
                        let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                        Error msg 
            Parser innerFn

        let run parser input = 
            let (Parser innerFn) = parser 
            innerFn input

        let andThen parser1 parser2 = 
            let innerFn input = 
                let result1 = run parser1 input 

                match result1 with 
                | Error err-> 
                    Error err
                | Ok (value1, remaining1) ->
                    let result2 = run parser2 remaining1
                    match result2 with 
                    | Error err ->
                        Error err 
                    | Ok (value2, remaining2) -> 
                        let newValue = (value1, value2)
                        Ok (newValue, remaining2)

            Parser innerFn
        let (.>>.) = andThen 

        let orElse parser1 parser2 = 
            let innerFn input = 
                let result1 = run parser1 input 
                match result1 with 
                | Ok _ -> 
                    result1 
                | Error _ -> 
                    let result2 = run parser2 input 
                    result2

            Parser innerFn
        let (<|>) = orElse 


        let parseA = pchar 'A'
        let parseB = pchar 'B'
        let parseC = pchar 'C'
        let bOrC = parseB <|> parseC
        let aAndThenBorC = parseA .>>. bOrC

        let demo01 () = 
            run aAndThenBorC "ABZ"

        // choose any of a list of parsers
        let choice listOfParsers = 
            List.reduce ( <|> ) listOfParsers

        // Choose any of a list of characters
        let anyOf listOfChars = 
            listOfChars
            |> List.map pchar 
            |> choice

        let parseLowercase =
          anyOf ['a'..'z']

        let parseDigit =
          anyOf ['0'..'9']

        let demo02 () = 
            run parseLowercase "aBC"  // Success ('a', "BC")

        let demo03 () =
            run parseDigit "9ABC"  // Success ("9", "ABC")

        let demo04 () =
            // Notice the error message is not correct.
            run parseLowercase "ABC"  // Failure "Expecting 'z'. Got 'A'" 

        let demo05 () = 
            // Notice the error message is not correct
            run parseDigit "|ABC"  // Failure "Expecting '9'. Got '|'"