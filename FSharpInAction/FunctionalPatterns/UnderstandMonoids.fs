namespace FunctionalPatterns
//From https://swlaschin.gitbooks.io/fsharpforfunandprofit/content/posts/monoids-part3.html
//Here are all the design tips together:

//To easily create a monoidal type, make sure that each field of the type is also a monoid.
//To enable closure for a non-numeric type, replace single items with lists (or a similar data structure).
//To get associativity for an operation, try to move the operation into the object.
//To get identity for an operation, create a special case in a discriminated union, or, even simpler, just use Option.

module UnderstandMonoids = 
    // Monoid means ICombinable: 
    // Rule 1 (Closure): The result of combining two things is always another one of the things.
    // Rule 2 (Associativity): When combining more than two things, which pairwise combination you do first doesn't matter.
    // Rule 3 (Identity element): There is a special thing called "zero" such that when you combine any thing with "zero" you get the original thing back.

    // Semigroups means something except Rule 3

    //To sum up, a monoid is basically a way to describe an aggregation pattern
    //we have a list of things, we have some way of combining them, 
    //and we get a single aggregated object back at the end.
    //Monoid Aggregation : 'T list -> 'T

    let demoBenefitOfCloureRule () = 
        // If we can define a pairwise operation, we can extend it to list operations "for free"

        // Explicit: 1 + 2 + 3 + 4
        // using reduce
        [ 1; 2; 3; 4 ] |> List.reduce (+) |> ignore 

        // [1] @ [2] @ [3] @ [4]
        [ [1]; [2]; [3]; [4] ] |> List.reduce (@) |> ignore 

    let demoBenefitOfAssociativityRule () = 
        // If the pairwise combinations can be done in any order, 
        // that opens up some interesting implementation techniques, such as
        // Divide and conquer algorithms; Parallelization and Incrementalism
        //          Core 1 	                    Core 2 	                    Core 3 	                Core 4
        //Step 1 	sum12 = 1 + 2 	            sum34 = 3 + 4 	            sum56 = 5 + 6 	        sum78 = 7 + 8
        //Step 2 	sum1234 = sum12 + sum34 	sum5678 = sum56 + sum78 	(idle) 	                (idle)
        //Step 3 	sum1234 + sum5678 	        (idle) 	                    (idle) 	                (idle)
        0 

    let demoBenefitOfIdentityRule () = 
        // It complement the edge cases in Semigroups
        [1..4] |> List.fold (*) 1

module MonoidsInPractise = 
    type OrderLine = 
        {
            ProductCode: string
            Qty: int
            Total: float
        }

    let orderLines = [
        {ProductCode="AAA"; Qty=2; Total=19.98}
        {ProductCode="BBB"; Qty=1; Total=1.99}
        {ProductCode="CCC"; Qty=3; Total=3.99}
    ]

    let demoSumV1 () = 
        let calculateOrderTotal lines = 
            let mutable total = 0.0
            for line in lines do
                total <- total + line.Total
            total

        orderLines 
        |> calculateOrderTotal 
        |> printfn "Total is %g"

    let demoSumV2 () = 
        // From mutable to use fold
        let calculateOrderTotal lines = 
            let accumulateTotal total line = 
                total + line.Total
            lines 
            |> List.fold accumulateTotal 0.0 

        orderLines 
        |> calculateOrderTotal 
        |> printfn "Total is %g"

    let demoSumV3 () =
        // Using monoid
        let addLine orderLine1 orderline2 = 
            {
                ProductCode = "Total"
                Qty = orderLine1.Qty + orderline2.Qty
                Total = orderLine1.Total + orderline2.Total
            }

        let printLine {ProductCode=p; Qty=q;Total=t} = 
            printfn "%-10s %5i %6g" p q t 

        // we can easily reuse the printLine function to make a simple receipt printing function that includes the total
        let printReceipt lines = 
            lines 
            |> List.iter printLine

            printfn "-----------------------"

            lines 
            |> List.reduce addLine
            |> printLine

        orderLines 
        |> printReceipt


        // Now use the incremental nature of monoids to keep a running subtotal that we update every time a new line is added. 
        let subtotal = orderLines |> List.reduce addLine 
        let newLine = {ProductCode="DDD"; Qty=1; Total=29.98}
        let newSubtotal = subtotal |> addLine newLine 
        newSubtotal |> printLine

        // Define a custom operator such as ++ so that we can add lines together naturally like number
        let (++) a b = addLine a b 
        (subtotal ++ newLine) |> printLine


     //let demoSumV4 () = 
     //   // Handle identy (zero) case 

    module MappingToDifferentStucture = 
        open System
        type Customer = {
            Name:string // and many more string fields!
            LastActive:DateTime 
            TotalSpend:float }

        // create a type to track customer statistics
        type CustomerStats = {
            // number of customers contributing to these stats
            Count:int 
            // total number of days since last activity
            TotalInactiveDays:int 
            // total amount of money spent
            TotalSpend:float }

        // All the fields in CustomerStats are numeric, so it is obvious how we can add two stats together:
        let add stat1 stat2 = {
            Count = stat1.Count + stat2.Count;
            TotalInactiveDays = stat1.TotalInactiveDays + stat2.TotalInactiveDays
            TotalSpend = stat1.TotalSpend + stat2.TotalSpend
            }

        let (++) a b = add a b
        
        
        // convert a customer to a stat
        let toStats cust =
            let inactiveDays= DateTime.Now.Subtract(cust.LastActive).Days;
            {Count=1; TotalInactiveDays=inactiveDays; TotalSpend=cust.TotalSpend}

        // create a list of customers
        let c1 = {Name="Alice"; LastActive=DateTime(2005,1,1); TotalSpend=100.0}
        let c2 = {Name="Bob"; LastActive=DateTime(2010,2,2); TotalSpend=45.0}
        let c3 = {Name="Charlie"; LastActive=DateTime(2011,3,3); TotalSpend=42.0}
        let customers = [c1;c2;c3]

        // Shows fold = map + reduce
        let foldFunc (acc: CustomerStats)  (customer: Customer) = 
            {
                Count = acc.Count + 1
                TotalInactiveDays = acc.TotalInactiveDays + (DateTime.Now.Subtract(customer.LastActive).Days)
                TotalSpend = acc.TotalSpend + customer.TotalSpend
            }

        let demoReduce () = 
            // aggregate the stats
            customers 
            |> List.map toStats
            |> List.reduce add
            |> printfn "result = %A"

        let demoReduceFailedForZeroCase () =                       
            []
            |> List.map toStats
            |> List.reduce add
            |> printfn "result = %A"
    

        let zero = {Count = 0; TotalInactiveDays = 0; TotalSpend = 0}
        let demoFold () = 
            customers 
            |> List.fold foldFunc zero
            |> printfn "result = %A"

        // Notice List.fold works on empty list.
        let demoFolderWorksWellWithZeroCase () =
            []
            |> List.fold foldFunc zero
            |> printfn "result = %A"
        

module MonoidHomomorphisms = 
    // A monoid homomorphism is a transformation that preserves an essential "monoidness", 
    // even if the "before" and "after" objects are quite different.

    // The definition of a monoid homomorphism:
    // Given a function that maps from one monoid to another (like 'wordCount' or 'mostFrequentWord')
    // Then to be a monoid homomorphism, the function must meet the requirement that:
    // function(chunk1) + function(chunk2) MUST EQUAL function(chunk1 + chunk2)

    // The advantage of the monoid homomorphism approach is that it is "chunkable". 

    open System

    type Text = 
        | Text of string 

    let addText (Text s1) (Text s2) = 
        Text (s1 + s2)
       
    let wordCount (Text s) = 
        System.Text.RegularExpressions.Regex.Matches(s, @"\S+").Count

    let page () =
        List.replicate 1000 "hello"
        |> List.reduce (+)
        |> Text

    let time f msg = 
        let stopwatch = Diagnostics.Stopwatch()
        stopwatch.Start()
        f () 
        stopwatch.Stop()

        printfn $"Time taken for {msg} was {stopwatch.ElapsedMilliseconds}ms."

    module WordCount = 
        let document () = 
            page () |> List.replicate 1000

        let demoWordCountV1 () = 
            // Add all the pages together using addText and then do a word count on the entire million word document.
            let wordCountViaAddText () = 
                document () 
                |> List.reduce addText
                |> wordCount
                |> printfn "The word count is %i"

            time wordCountViaAddText "reduce then count"

        let demoWordCountV2 () = 
            // we'll do wordCount on each page first, and then add all the results together 
            let wordCountViaMap () = 
                document ()
                |> List.map wordCount
                |> List.reduce (+)
                |> printfn "The word count is %i"

            time wordCountViaMap "map then reduce"

        let demoWordCountV3 () = 
            // Use parallell
            let wordCountViaParallelAddCounts () = 
                document () 
                |> List.toArray
                |> Array.Parallel.map wordCount
                |> Array.reduce (+)
                |> printfn "The word count is %i"

            time wordCountViaParallelAddCounts "parallel map then reduce"


    module FrequentWord = 
        open System.Text.RegularExpressions

        let page1() = 
            List.replicate 1000 "hello world "
            |> List.reduce (+)
            |> Text

        let page2() = 
            List.replicate 1000 "goodbye world "
            |> List.reduce (+)
            |> Text

        let page3() = 
            List.replicate 1000 "foobar "
            |> List.reduce (+)
            |> Text

        let document() = 
            [page1(); page2(); page3()]

        let mostFrequentWord (Text s) =
            Regex.Matches(s,@"\S+")
            |> Seq.cast<Match> // Convert IEnumerable<Match> to Seq<Match>
            |> Seq.map (fun m -> m.ToString())
            |> Seq.groupBy id
            |> Seq.map (fun (k,v) -> k, Seq.length v)
            |> Seq.sortBy (fun (_,v) -> -v)
            |> Seq.head
            |> fst

        let demoFrequentWordV1 () = 
             document() 
            |> List.reduce addText
            |> mostFrequentWord
            |> printfn "Using add first, the most frequent word is %s"
        
        let demoFrequentWordV2 () = 
            // Notice: this gives completely wrong result!
            // The mapping transformed a monoid (Text) to another monoid (string).
            // The shape is not preserved !
            // In other words, it is not a proper monoid homomorphism.
            document() 
            |> List.map mostFrequentWord
            |> List.reduce (+)
            |> printfn "Using map reduce, the most frequent word is %s"


module WorkingWithNonMonoids = 
    // DESIGN TIP: To easily create a monoidal type, make sure that each field of the type is also a monoid.
    // Question to think about: when you do this, what is the "zero" of the new compound type?
    // DESIGN TIP: To enable closure for a non-numeric type, replace single items with lists.
    open System

    module MonoidalChar = 
        /// "monoidal char"
        type MChar = MChar of Char list

        /// convert a char into a "monoidal char"
        let toMChar ch = MChar [ch]

        /// add two monoidal chars
        let addChar (MChar l1) (MChar l2) = 
            MChar (l1 @ l2)

        // infix version
        let (++) = addChar  

        /// convert to a string
        let toString (MChar cs) = 
            new System.String(List.toArray cs)


        let demoMonoidalChar () = 
            // add two chars and convert to string
            let a = 'a' |> toMChar
            let b = 'b' |> toMChar
            let c = a ++ b
            c |> toString |> printfn "a + b = %s"  
            // result: "a + b = ab"

    module MonoidalValidation =
        // Show how to convert non-monoid to monoid (for closure rule) using list

        type ValidationResult = 
            | Success
            | Failure of string list

        // helper to convert a single string into the failure case
        let fail str =
            Failure [str]

        let validateBadWord (badWord: string) (name:string) =
            if name.Contains(badWord) then
                fail ("string contains a bad word: " + badWord)
            else 
                Success 

        let validateLength maxLength name =
            if String.length name > maxLength then
                fail "string is too long"
            else 
                Success

        // By definition, it is something that when combined with another result, leaves the other result alone.
        let zero = Success

        /// add two results
        let add r1 r2 = 
            match r1,r2 with
            | Success,    Success -> Success 
            | Failure f1, Success -> Failure f1
            | Success,    Failure f2 -> Failure f2
            | Failure f1, Failure f2 -> Failure (f1 @ f2)

        let test1() =
            let result1 = Success
            let result2 = Success
            add result1 result2 
            |> printfn "Result is %A"
            // "Result is Success"

        let test2 () = 
            let result1 = Success
            let result2 = fail "string is too long"
            add result1 result2 
            |> printfn "Result is %A"
            // "Result is Failure ["string is too long"]"

        let test3 () = 
            let result1 = fail "string is null or empty"
            let result2 = fail "string is too long"
            add result1 result2 
            |> printfn "Result is %A"

            // Result is Failure 
            //   [ "string is null or empty"; 
            //     "string is too long"]

        let test4 () = 
            let validationResults str = 
                [
                    validateLength 10
                    validateBadWord "monad"
                    validateBadWord "cobol"
                ]
                |> List.map (fun validate -> validate str)

            "cobol has native support for monads"
            |> validationResults 
            |> List.fold add zero  // using fold instead of reduce we now could support empty list
            |> printfn "Result is %A"



    module MonoidForAssociativity = 
        // DESIGN TIP: To get associativity for an operation, try to move the operation into the object.
        // This is like say 5 - 3 => 5 + (-3), such that (-) operation becomes (+) operation which is now associative

        module NonAssociative = 
            let subtractChars (s1:string) (s2:string) = 

                let isIncluded (ch:char) = 
                    s2.IndexOf(ch) = -1

                let chars = s1.ToCharArray() |> Array.filter isIncluded
                System.String(chars)

            let (--) = subtractChars

            let demoAssociativity () = 
                let result1 = ("abc" -- "abc") -- "abc"
                let result2 = "abc" -- ("abc" -- "abc")
                result1 <> result2


        module AssociativeV1 = 
            /// store a list of chars to remove
            type CharsToRemove = CharsToRemove of Set<char>

            /// construct a new CharsToRemove
            /// We are modelling actions rather than data. We have a list of CharsToRemove actions, then we combine them into a single "big" CharsToRemove action,
            /// and then we execute that single action at the end, after we have finished the intermediate manipulations.
            let subtract (s:string) = 
                s.ToCharArray() |> Set.ofArray |>  CharsToRemove 

            /// apply a CharsToRemove to a string
            let applyTo (s:string) (CharsToRemove chs) = 
                let isIncluded ch = 
                    Set.exists ((=) ch) chs |> not
                let chars = 
                    s.ToCharArray() |> Array.filter isIncluded
                System.String(chars)

            let (++) (CharsToRemove c1) (CharsToRemove c2) = 
                CharsToRemove (Set.union c1 c2)

            let test01 = 
                (subtract "abd") 
                |> applyTo "abcdef" |> printfn "Result is %s"

            let test02 = 
                let removalAction = (subtract "abc") ++ (subtract "def") ++ (subtract "abc")   
                removalAction |> applyTo "abcdef" |> printfn "Result is %s"

        module AssociativeV2 = 
            // This is the functional equivalent of creating the CharsToRemove data structure.  
            // Note that we reverse the parameters to make partial application easier
            let subtract str charsToSubtract = 
                NonAssociative.subtractChars charsToSubtract str 

            let test1 = 
                let removalAction = subtract "abd" 
                "abcdef" |> removalAction |> printfn "Result is %s"

            let test2 = 
                let removalAction2 = (subtract "abc") >> (subtract "de") >> (subtract "abc") 
                removalAction2 "abcdef" |> printfn "Result is %s"


    module MonoidForIdentity = 
        //DESIGN TIP: To get identity for an operation, create a special case in a discriminated union, or, even simpler, just use Option.

        module PositiveNumbersV1 = 
            type NormalOrIdentity<'T> = 
                | Normal of 'T
                | Zero 

            let optionAdd f o1 o2 =
                match o1, o2 with
                | None, _ -> o2
                | _, None -> o1
                | Some s1, Some s2 -> Some (f s1 s2)


        module PositiveNumbersV2 = 
            type PositiveNumberOrIdentity = int option
            let addPositive = PositiveNumbersV1.optionAdd (+)

            let zero = None

            let test1 = 
                [1..10]
                |> List.map Some
                |> List.fold addPositive zero 

            let test2 = 
                []
                |> List.map Some
                |> List.fold addPositive zero


    module Average = 
        let avgf i1 i2 = 
            float (i1 + i2) / 2.0
        // Change average number to be monoid: 
        // By default, it is not closure (int -> float); it is not associative; there is no identity.
        // Apply the design tips
        // 1) To easily create a monoidal type, make sure that each field of the type is also a monoid.
        // 2) To enable closure for a non-numeric type, replace single items with lists.
        // 3) To get associativity for an operation, try to move the operation into the object.
        // 4) To get identity for an operation, create a special case in a discriminated union, or, even simpler, just use Option.

        // How do we convert "average" from a verb (an operation) to a noun (a data structure)?
        // The answer is that we create a structure that is not actually an average, 
        // but a "delayed average" -- everything you need to make an average on demand.
        type Avg = {total: int; count: int}

        let addAvg avg1 avg2 = 
            {
                total = avg1.total + avg2.total
                count = avg1.count + avg2.count
            }

        let zero = {total = 0; count = 0}

        let (++) = addAvg

        // convert a single number to average
        let avg n = {total = n; count = 1}

        let calcAvg avg = 
            if avg.count = 0 then 
                0.0
            else 
                float (avg.total / avg.count)


        let test1 = 
            addAvg (avg 4) (avg 5)
            |> calcAvg
            |> printfn "Average is %A"

        let test2 = 
            (avg 4) ++ (avg 5) ++ (avg 6) 
            |> calcAvg 
            |> printfn "Average is %A"

        let test3 =
            [1..10]
            |> List.map avg 
            |> List.reduce addAvg
            |> calcAvg
            |> printfn "Average is %A"

    module MostFrequentWord = 
        // The insight here is again to delay the calculation until the last minute, just as we did in the "average" example
        // Rather than calculating the most frequent word upfront then, 
        // We create a data structure that stores all the information that we need to calculate the most frequent word later.

        open System 
        open System.Text.RegularExpressions

        type Text = Text of string 

        let page1() = 
            List.replicate 1000 "hello world "
            |> List.reduce (+)
            |> Text

        let page2() = 
            List.replicate 1000 "goodbye world "
            |> List.reduce (+)
            |> Text

        let page3() = 
            List.replicate 1000 "foobar "
            |> List.reduce (+)
            |> Text

        let document() = 
            [page1(); page2(); page3()]

        let addText (Text s1) (Text s2) = 
            Text (s1 + s2)

        let wordFreq (Text s) = 
            Regex.Matches(s, @"\S+")
            |> Seq.cast<Match>
            |> Seq.map (fun m -> m.ToString())
            |> Seq.groupBy id
            |> Seq.map (fun (k, v) -> k, Seq.length v)
            |> Map.ofSeq

        let testFreqFeature = 
            page1() |> wordFreq |> printfn "The frequency map for page1 is %A"
            page2() |> wordFreq |> printfn "The frequency map for page2 is %A"

            document() 
            |> List.reduce addText
            |> wordFreq 
            |> printfn "The frequency map for the document is %A"

        // Add two maps of frequence of words
        let addMap map1 map2 = 
            let increment mapSoFar word count = 
                match mapSoFar |> Map.tryFind word with 
                | Some count' -> mapSoFar |> Map.add word (count + count')
                | None -> mapSoFar |> Map.add word count

            map2 |> Map.fold increment map1 

        let mostFrequentWord map = 
            let max (candidateWord, maxCountSoFar) word count = 
                if count > maxCountSoFar then 
                    (word, count)
                else 
                    (candidateWord, maxCountSoFar)

            map |> Map.fold max ("None", 0)

        let test1 = 
            document() 
            |> List.reduce addText
            |> wordFreq
            // get the most frequent word from the big map
            |> mostFrequentWord
            |> printfn "Using add first, the most frequent word and count is %A"

        let test2 = 
            document() 
            |> List.map wordFreq
            |> List.reduce addMap
            // get the most frequent word from the merged smaller maps
            |> mostFrequentWord
            |> printfn "Using map reduce, the most frequent and count is %A"
            
        let testMapFold = 
            let foldF mapAcc k v = 
                mapAcc |> Map.add k (v-10)

            let anotherMap = 
                [1..100]
                |> Seq.map (fun i -> i, -i)
                |> Map.ofSeq

            [1..6]
            |> Seq.map (fun i -> i, i * i)
            |> Map.ofSeq
            |> Map.fold foldF anotherMap

