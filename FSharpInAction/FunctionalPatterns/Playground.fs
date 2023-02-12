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

        let (.>>.) = andThen

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

        let (<|>) = orElse 

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

        let ( |>> ) x f = mapP f x 

        // 4.2 We want to Lift any number parameter function to Parser world
        // Make 'a to Parser<'a>
        let returnP x =
            let innFunc input = 
                Ok (x, input)
            Parser innFunc
        // 4.3 
        let applyP fp xP = 
            (fp .>>. xP)
            |> mapP (fun (f, x) -> f x)
        let (<*>) = applyP
        
        // 4.4
        // With returnP and applyP, let build one which a two parameter function to Parser world
        let lift2 f xp yP =
            returnP f <*> xp <*> yP
        let lift2V2 f xP yP =
            let tmp1 = applyP f xP 
            let tmp2 = applyP tmp1 yP
            returnP tmp2

        // 5. Use lift2 to build custom parsers by combine some parsers
        // 5.1
        let addP = lift2 ( + )
        
        // 5.2
        let startWith (str: string) (prefix: string) =
            str.StartsWith(prefix)
        // Notice: we are combine two string parser to get a bool parser! 
        let startsWithP =
            lift2 startWith
