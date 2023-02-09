﻿namespace FunctionalPatterns

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

