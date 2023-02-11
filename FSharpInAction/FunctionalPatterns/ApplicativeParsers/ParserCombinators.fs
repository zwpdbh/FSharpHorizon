namespace FunctionalPatterns.ApplicativeParsers

module ParserCombinators =
    // Continue on ParserFoundations: Build out the library with many other useful combinators. 
    open System

    type ParseResult<'a> = 
            | Ok of 'a 
            | Error of string

    // The fact that a Parser can contain any type, not just a char or string, is a key feature.
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

    // choose any of a list of parsers
    let choice listOfParsers = 
        List.reduce ( <|> ) listOfParsers

    // Choose any of a list of characters
    let anyOf listOfChars = 
        listOfChars
        |> List.map pchar 
        |> choice

    
    let notWork01 () =
        //let pstring str =
        //    str
        //    |> Seq.map pchar // convert into parsers
        //    |> Seq.reduce andThen
        0

    // Lift a one parameter function from normal world into Parser world
    let mapP f parser = 
        let innerFn input = 
            let result = run parser input 

            match result with 
            | Ok (v, remaining) ->
                let v' = f v 
                Ok (v', remaining)

            | Error err -> 
                Error err
        Parser innerFn
 
    // Notice the swap for binary operator definition.
    let ( |>> ) x f = mapP f x 

    // Why need returnP and applyP ?
    // "map" will lift functions in Normal World into functions in Parser World, but only for one-parameter functions.
    // They can lift any function in Normal World into a function in Parser World, no matter how many parameters it has.
    let returnP x =
        let innerFn input =
            Ok (x, input)
        Parser innerFn
        
    let applyP fP xP = 
        (fP .>>. xP)
        |> mapP (fun (f, x) -> f x)

    let (<*>) = applyP

    // Lift a two parameter function into Parser world
    let lift2 f xP yP =
        returnP f <*> xP <*> yP 

    // Now we could lift any two parameter functions into Parser world!
    let addP = 
        lift2 ( + )

    let startsWith (str:string) (prefix:string) =
        str.StartsWith(prefix)
        
    let startsWithP =
        lift2 startsWith 

    let demeTransformingWithMap () = 
        let parseDigit = anyOf ['0'..'9']
        let parseThreeDigitsAsStrV1 = 
            let tupleParser = 
                parseDigit .>>. parseDigit .>>. parseDigit 

            let transformTuple ((c1, c2), c3) = 
                System.String [|c1; c2; c3|]

            mapP transformTuple tupleParser

        let parseThreeDigitsAsStrV2 = 
            (parseDigit .>>. parseDigit .>>. parseDigit)
            |>> fun ((c1, c2), c3) -> System.String [| c1; c2; c3 |]

        let parseThreeDigitsAsInt =
            mapP int parseThreeDigitsAsStrV2

        run parseThreeDigitsAsStrV2 "123A" |> printfn "%A" // Success ("123", "A")
        run parseThreeDigitsAsInt "123A" |> printfn "%A" // Success (123, "A")

    // Now we are ready to create a list of parsers
    let rec sequence parserList = 
        let cons head tail = head :: tail 

        let consP = lift2 cons 
        match parserList with 
        | [] ->
            returnP []
        | head::tail -> 
            consP head (sequence tail)

    let demoLifting () =
        let parsers = [ pchar 'A'; pchar 'B'; pchar 'C' ]
        let combined = sequence parsers
            
        run combined "ABCD" |> printfn "%A" // Success (['A'; 'B'; 'C'], "D")

    // Ref: https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-2/#implementing-the-pstring-parser

    let pstring (str: string) =
        str 
        |> List.ofSeq
        |> List.map pchar
        |> sequence
        |> mapP (fun charList ->
            charList |> List.toArray |> System.String
        )

    let demoPString () =
        let parseABC = pstring "ABC"
        
        run parseABC "ABCDE" |> ignore  // Success ("ABC", "DE")
        run parseABC "A|CDE" |> ignore // Failure "Expecting 'B'. Got '|'"
        run parseABC "AB|DE" |> ignore // Failure "Expecting 'C'. Got '|'"

    
    // Ref: https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-2/#4-matching-a-parser-multiple-times
    let rec parseZeroOrMore parser input = 
        let firstResult = run parser input 
        match firstResult with 
        | Error _ ->
            // If the parser returns Failure (and this is key) just return an empty list. That is, this function can never fail!
            ([], input)
        | Ok (firstValue, inputAfterFirstParse) ->
            let (subsequentValues, remainingInput) = 
                parseZeroOrMore parser inputAfterFirstParse
            let values = firstValue :: subsequentValues
            (values, remainingInput)

    let many parser = 
        let innerFn input = 
            // parse the input -- wrap in Success as it always succeeds
            Ok (parseZeroOrMore parser input)

        Parser innerFn

    let demoMany () = 
        let manyA = many (pchar 'A')
        
        // test some success cases
        run manyA "ABCD" |> ignore // Success (['A'], "BCD")
        run manyA "AACD" |> ignore // Success (['A'; 'A'], "CD")
        run manyA "AAAD" |> ignore // Success (['A'; 'A'; 'A'], "D")       
        // test a case with no matches
        run manyA "|BCD" |> ignore // Success ([], "|BCD")

        let manyAB = many (pstring "AB")  
        run manyAB "ABCD" |> ignore // Success (["AB"], "CD")
        run manyAB "ABABCD" |> ignore // Success (["AB"; "AB"], "CD")
        run manyAB "ZCD"  |> ignore// Success ([], "ZCD")
        run manyAB "AZCD" |> ignore // Success ([], "AZCD")

        let whitespaceChar = anyOf [' '; '\t'; '\n']
        let whitespace = many whitespaceChar
        run whitespace "ABC"  |> ignore// Success ([], "ABC")
        run whitespace " ABC" |> ignore// Success ([' '], "ABC")
        run whitespace "\tABC" |> ignore // Success (['\t'], "ABC")

    let many1 parser = 
        let innerFn input = 
            let firstResult = run parser input 
            match firstResult with 
            | Error err -> 
                Error err 
            | Ok (firstValue, inputAfterFirstParse) -> 
                let (subsequentyValues, remainingInput) = parseZeroOrMore parser inputAfterFirstParse
                let values = firstValue::subsequentyValues
                Ok (values, remainingInput)

        Parser innerFn

    let demoMany1 () =
        // define parser for one digit
        let digit = anyOf ['0'..'9']
        
        // define parser for one or more digits
        let digits = many1 digit
        
        run digits "1ABC" |> ignore // Success (['1'], "ABC")
        run digits "12BC" |> ignore // Success (['1'; '2'], "BC")
        run digits "123C" |> ignore // Success (['1'; '2'; '3'], "C")
        run digits "1234" |> ignore // Success (['1'; '2'; '3'; '4'], "")
        
        run digits "ABC"  |> ignore // Failure "Expecting '9'. Got 'A'"

    // Parsing an integer
    let pint = 
        let resultToInt digitList =
            digitList |> List.toArray |> System.String |> int 

        let digit = anyOf ['0'..'9']
        let digits = many1 digit 
        digits
        |> mapP resultToInt

    let demoParseInt () =
        run pint "1ABC"  |> ignore // Success (1, "ABC")
        run pint "12BC"  |> ignore // Success (12, "BC")
        run pint "123C"  |> ignore // Success (123, "C")
        run pint "1234"  |> ignore // Success (1234, "")
        
        run pint "ABC"   |> ignore // Failure "Expecting '9'. Got 'A'"

    // Ref: https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-2/#5-matching-a-parser-zero-or-one-time