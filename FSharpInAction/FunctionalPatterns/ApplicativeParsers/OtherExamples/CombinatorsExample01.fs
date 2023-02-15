namespace OtherTopics
open Expecto

module Combinators = 
    // For https://www.youtube.com/watch?v=ARJB8eDyxrg

    type Parser<'a> = 
        | Parser of (char list -> Result<'a * char list, string>)
    
    let runParser parser inputChars = 
        match parser with 
        | Parser parserFun -> 
            parserFun inputChars

    let expectChar expChar = 
        let innerParser inputChars = 
            match inputChars with 
            | c :: remainingChars ->
                if c = expChar then 
                    Result.Ok (c, remainingChars)
                else 
                    Result.Error ($"Expected {expChar}, got {c}")
            | _ -> 
                Result.Error ($"Expected {expChar}, but reach end of inputChar")

        Parser innerParser

    let strToCharList str =
        str |> List.ofSeq

    // The idea of combinator: combine two parser function to achieve more complex behaviour. 

    // Or Parser
    let orParse pFun1 pFun2 = 
        let innerParser inputChars = 
            match runParser pFun1 inputChars with 
            | Result.Ok result -> Result.Ok result 
            | Result.Error _ -> 
                runParser pFun2 inputChars
        Parser innerParser

    // Define operator 
    // Operator definitions are left associative 
    let ( <|> ) = orParse

    let choice parserList = 
        List.reduce orParse parserList

    let anyCharOf validChars = 
        validChars
        |> List.map expectChar
        |> choice

    let andParse pFun1 pFun2 = 
        let innerParser inputChars = 
            match runParser pFun1 inputChars with 
            | Result.Ok (c1, remaining1) -> 
                match runParser pFun2 remaining1 with 
                | Result.Ok (c2, remaining2) -> 
                    Result.Ok ((c1, c2), remaining2)
                | Result.Error err -> Result.Error err 
            | Result.Error err -> Result.Error err 

        Parser innerParser

    let ( .>>. ) = andParse

    let mapParser mapFun parser = 
        let innerParser inputChars = 
            match runParser parser inputChars with 
            | Result.Error msg -> Result.Error msg 
            | Result.Ok (result, remainingChars) ->
                Result.Ok (mapFun result, remainingChars)

        Parser innerParser

    let applyParser funcAsParser paramAsParser = 
        (funcAsParser .>>. paramAsParser)
        |> mapParser (fun (f, x) -> f x)

    let ( <*> ) = applyParser

    let returnAsParser result = 
        let innerParser inputChars = 
            Result.Ok (result, inputChars)

        Parser innerParser

    let liftToParser2 funcToLift paramAsParser1 paramAsParser2 = 
        returnAsParser funcToLift <*> paramAsParser1 <*> paramAsParser2

    let rec sequenceParsers parserList = 
        let cons head rest = head :: rest 
        let consAsParser = liftToParser2 cons 

        match parserList with 
        | parser :: remainerParsers -> 
            consAsParser parser  (sequenceParsers remainerParsers)
        | [] -> 
            returnAsParser []


    let expectString expectString = 
        expectString
        |> strToCharList
        |> List.map expectChar
        |> sequenceParsers
        |> mapParser (fun chars -> System.String(List.toArray chars))

module CombinatorsDemos = 
    open Combinators

    // OR 
    let demo01 () = 
        strToCharList "rake"
        |> runParser (expectChar 'r' <|> expectChar 't' <|> expectChar 'c') 

    let demo02 () = 
        strToCharList "rake"
        |> runParser (choice [(expectChar 'r'); (expectChar 'a')])

    let demo03 () = 
        strToCharList "wake"
        |> runParser (anyCharOf ("rkaw" |> List.ofSeq))


    // AND 
    let demo04 () = 
        strToCharList "rake"
        |> runParser (andParse (expectChar 'r') (expectChar 'a'))

    let demo05 () = 
        strToCharList "rake"
        |> runParser ((expectChar 'r' .>>. expectChar 'a'))

    let demo06 () = 
        strToCharList "rake"
        |> runParser (expectChar 'r')

    let demo07 () = 
        strToCharList "rake"
        |> runParser (expectString "rake")

    let demo08 () = 
        strToCharList "rake"
        |> runParser (expectString "rake" <|> expectString "lake")