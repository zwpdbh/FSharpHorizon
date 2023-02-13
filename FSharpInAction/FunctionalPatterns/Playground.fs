namespace FunctionalPatterns
module Playground =
    
    module MostFrequentWord = 
        open System.Text.RegularExpressions
        type Text = Text of string 

        let page1 = 
            List.replicate 1000 "z h a o w e i"
            |> List.reduce ( + )
            |> Text 

        let page2 = 
            List.replicate 1000 "z w p d b h"
            |> List.reduce ( + )
            |> Text 

        let document = [page1; page2]

        let wordFreq (Text s) = 
            Regex.Matches(s, @"\S+")
            |> Seq.cast<Match>
            |> Seq.map (fun x -> x.ToString())
            |> Seq.groupBy id 
            |> Seq.map (fun (k, v) -> k, Seq.length v)
            |> Map.ofSeq

        let addMap m1 m2 = 
            let increment mapSofar k v = 
                match mapSofar |> Map.tryFind k with 
                | None -> 
                    mapSofar |> Map.add k v 
                | Some v' -> 
                    mapSofar |> Map.add k (v + v')

            m1 |> Map.fold increment m2 

        let mostFrequentWord m =
            let increment (candidate, maxCount) word count =
                if count > maxCount then 
                    (word, count)
                else 
                    (candidate, maxCount)

            m |> Map.fold increment ("none", -1)

        let test() =
            document
            |> List.map wordFreq
            |> List.reduce addMap 
            |> mostFrequentWord
            |> printfn "%A"
            

    module ParserFoundations = 
        open System

        type ParseResult<'a> =
            | Ok of 'a 
            | Error of string 

        // 1. Realize that Function is type. Type could be function (not just some data structure)
        type Parser<'a> = Parser of (string -> ParseResult<'a * string>)

        // 2. We need to use partial function to create some specific parser
        // In general, we want to give a char, we want to produce a function which when receive input, it will produce the result.
        let pchar charToMatch  = 
            // How to convert a normal function to a type
            // 2.1.1 Use innerFunc (rely on partial function)
            let innerFunc str = 
                if String.IsNullOrEmpty(str) then 
                    Error "No more input"
                else 
                    if charToMatch = str[0] then 
                        Ok (charToMatch, str[1..])
                    else 
                        let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch str[0]
                        Error msg 
            // 2.1.2 Wrap the function with Type
            Parser innerFunc 

        // Before we could directly run the parser (the function) such as "parseA "ZBC"
        // 2.2 Define a helper function which unwrap it first, then run it. 
        let run parser input = 
            let (Parser innerFunc) = parser
            innerFunc input

        // In 3, we build AND, OR parser combinator
        // 3.1 And logic: build a parser which parse a and b 
        let andThen parser1 parser2 = 
            let innerFunc input = 
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

            Parser innerFunc

        /// And logic, andThen parser
        let ( .>>. ) = andThen

        // 3.2
        // Or logic: build a parser which parse a or b 
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

        /// Or logic, orElse parser
        let ( <|> ) = orElse 

        // 3.3
        // With AND and OR logic in our hand, 
        // Let's build AnyOf which match any one of char from a char list.
        let anyOf listOfChars =
            listOfChars
            |> List.map pchar 
            |> List.reduce (<|>) 

        let parseLowercase =
          anyOf ['a'..'z']

        let parseDigit =
          anyOf ['0'..'9']


        let testParserCreatedFromAnyOf() = 
            run parseLowercase "baBC" 
            |> printfn "%A"

        // 4. Lifting
        // 4.1 Lift one parameter function
        let mapP f parser = 
            let innerFunc input = 
                match run parser input with 
                | Ok (v, remaining) ->
                    Ok (f v, remaining)
                | Error err ->
                    Error err 

            Parser innerFunc
        
        /// Lifting, mapP f x 
        let ( |>> ) x f = mapP f x 

        // 4.2 We want to Lift any number parameter function to Parser world
        // Make 'a to Parser<'a>
        let returnP x =
            let innerFunc input = 
                Ok (x, input)
            Parser innerFunc
        // 4.3 
        let applyP fp xP = 
            (fp .>>. xP)
            |> mapP (fun (f, x) -> f x)
        let (<*>) = applyP
        
        // 4.4
        // With returnP and applyP, let build one which a two parameter function to Parser world
        let map2P f xp yP =
            returnP f <*> xp <*> yP
        let lift2V2 f xP yP =
            let tmp1 = applyP f xP 
            let tmp2 = applyP tmp1 yP
            returnP tmp2

        // 5. Use lift2 to build custom parsers by combine some parsers
        // 5.1
        let addP = map2P ( + )
        
        // 5.2
        let startWith (str: string) (prefix: string) =
            str.StartsWith(prefix)
        // Notice: we are combine two string parser to get a bool parser! 
        let startsWithP =
            map2P startWith

        // 5.3 To define a parser which parse a prefix string and return prefix + remaining both as string.
        // 5.3.1 We cound define a function which transform ((c1, c2), c3) to System.String [|c1; c2; c3|]. Thus, it could parse 3 digits.
        let parse3Digits = 
            let parseDigit = anyOf ['0'..'9']
            let tupleParser = 
                parseDigit .>>. parseDigit .>>. parseDigit

            let transformTupleCharToString ((c1, c2), c3) = 
                System.String [|c1; c2; c3|]

            // Lift it using one parameter lifting
            mapP transformTupleCharToString tupleParser

        let testParse3Digits () =
            run parse3Digits "1223A" |> printfn "%A"

        // 5.3.2 We really want to define a parser which could parse a list of digits (as many digits as it could)
        // We need to use recursion
        let rec sequence parserList =
            let cons head tail = head :: tail 
            let consP = map2P cons 

            match parserList with 
            | [] ->
                returnP []
            | head::tail -> 
                consP head (sequence tail)

        let testSequenceParser () = 
            [ pchar 'A'; pchar 'B'; pchar 'C'; pchar 'D' ]
            |> sequence
            |> run <| "ABCD12345"
            |> printfn "%A"
            
        // Ref: https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-2/#implementing-the-pstring-parser
        // 5.4 Implement a string parser
        let pstring (str: string) = 
            str 
            |> List.ofSeq
            |> List.map pchar 
            |> sequence
            |> mapP (fun charList -> 
                // use mapP to lift this lambda function into parser world
                charList |> List.toArray |> System.String
            )

        let testingPstring () =
            pstring "zhaowei"
            |> run <| "zhaowei is good" 
            |> printfn "%A"
            

        // 5.5 match a parser multiple times
        let rec parseZeroOrMore parser input = 
            let result = run parser input 

            match result with 
            | Ok (x, remainingFromThisParsing) ->
                let (y, remainingFromFailedToParse) = parseZeroOrMore parser remainingFromThisParsing  
                (x :: y, remainingFromFailedToParse)
            | Error _ ->
                ([], input)

        /// A parser matches 0 or as many as parser<'a>, like .*
        let many parser = 
            let innerFunc input = 
                Ok (parseZeroOrMore parser input)

            Parser innerFunc

        /// A parser match 1 or as many as parser<'a>, like .+
        /// The idea utilizing parseZeroOrMore: many1 = match once + many
        let many1 parser = 
            let innerFunc input = 
                match run parser input with 
                | Error err -> 
                    Error err 
                | Ok (x, remainingAfterThisParsing) ->                    
                    let (y, restAfterParsing) = parseZeroOrMore parser remainingAfterThisParsing
                    Ok (x::y, restAfterParsing)

            Parser innerFunc

        let testMany () = 
            many (pchar 'a')
            |> run <| "aaaaabc" 
            |> printfn "%A"

            many (pstring "zhaowei")
            |> run <| "zhaoweizhaoweizhao"
            |> printfn "%A"

            many (pstring "noexist")
            |> run <| "zhaoweizhaoweizhao"
            |> printfn "%A"

            many1 (pstring "zhaowei")
            |> run <| "zhaoweizhaoweizhao"
            |> printfn "%A"

            many1 (pstring "noexist")
            |> run <| "zhaoweizhaoweizhao"
            |> printfn "%A"

            let whitespaceChar = anyOf [' '; '\t'; '\n']
            let whitespace = many whitespaceChar

            ["ABC"; " ABC"; "\tABC"]
            |> List.iter (fun each ->
                 each |> run whitespace |> printfn "%A"
            )

        let digit = anyOf ['0'..'9']
        /// define parser for one or more digits
        let pDigits = many1 digit

        let testDigit () =       
            run pDigits "1ABC" |> printfn "%A"
            run pDigits "ABC" |> printfn "%A"
            run pDigits "121212ABC" |> printfn "%A"

        /// parse out integer from string
        let pInt =
            let resultToInt digitList = 
                digitList |> List.toArray |> System.String |> int

            pDigits
            |> mapP resultToInt 
           
        let testPInt () =       
            run pInt "1ABC" |> printfn "%A"
            run pInt "ABC" |> printfn "%A"
            run pInt "121212ABC" |> printfn "%A" 


        //5.6 Matching a parser 0 or 1 time
        //General idea is 
        // 1) map the result to Some
        // 2) create parser always return None
        // 3) use Or "<|>" operator to choose None parser if the first fail
        let opt p =
            let some = p |>> Some 
            let none = returnP None 
            some <|> none 
        
        let pInteger =
            let resultToInt (sign, charList) = 
                let i = charList |> List.toArray |> System.String |> int 
                match sign with 
                | Some _ -> -i 
                | None -> i 

            let digit = anyOf ['0'..'9']
            let digits = many1 digit 

            (opt (pchar '-') .>>. digits)
            |> mapP resultToInt 

        let testNegativeInteger () = 
            run pInteger "-121212ABC" |> printfn "%A" 


        /// 6. We want to parse something out, but pick something from parsed result
        /// Or skip/throw away something from the parsed result
        /// Ref: https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-2/#6-throwing-results-away