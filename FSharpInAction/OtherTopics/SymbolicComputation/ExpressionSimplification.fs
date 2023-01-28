namespace FunAndProfit
open Expecto

module ExpressionSimplification = 
    open System
    //  Implementation of symbolic differentiation over this simple expression type.
    // Define the precedence of operator

    type Expr = 
        | Var 
        | Num of int 
        | Sum of Expr * Expr 
        | Prod of Expr * Expr 

        override x.ToString() = 
            let precSum = 10 
            let precProd = 20
            let rec stringOfExpr prec expr = 
                match expr with 
                | Var -> "x"
                | Num i -> i.ToString() 
                | Sum (e1, e2) -> 
                    let sum = stringOfExpr precSum e1 + "+" + stringOfExpr precSum e2 
                    if prec > precSum then 
                        "(" + sum + ")"
                    else 
                        sum 
                | Prod (e1, e2) -> 
                    stringOfExpr precProd e1 + "*" + stringOfExpr precProd e2      
            stringOfExpr 0 x 

         

    let rec deriv expr = 
        match expr with 
        | Var -> Num 1 
        | Num _ -> Num 0 
        | Sum (e1, e2) -> Sum (deriv e1, deriv e2)
        | Prod (e1, e2) -> Sum (Prod (e1, deriv e2), Prod (e2, deriv e1))



    // Intrinsic type extensions for ToString is deprecated, so we better define it with type. 
    //type Expr with 
    //    override x.ToString() = 
    //        stringOfExpr 0 x 

    // Implementing Local Simplifications
    // Such that we don't have to do so. 
    let simpSum (a, b) = 
        match a, b with 
        | Num n, Num m -> Num (n+m)
        | Num 0, e | e, Num 0 -> e 
        | e1, e2 -> Sum(e1, e2)

    let simpProd (a, b) = 
        match a, b with 
        | Num n, Num m -> Num (n * m)
        | Num 0, e | e, Num 0 -> Num 0 
        | Num 1, e | e, Num 1 -> e 
        | e1, e2 -> Prod (e1, e2)

    let rec simpDeriv e = 
        match e with 
        | Var -> Num 1 
        | Num _ -> Num 0 
        | Sum (e1, e2) -> simpSum (simpDeriv e1, simpDeriv e2)
        | Prod (e1, e2) -> simpSum (simpProd (e1, simpDeriv e2), simpProd (e2, simpDeriv e1))


    let test01 = 
        testCase "Test simple derivative"
        <| fun _ -> 
        
            let e1 = Sum (Num 1, Prod (Num 2, Var))
            let e2 = deriv e1 

            Expect.equal e2 (Sum (Num 0,Sum (Prod (Num 2,Num 1),Prod (Var,Num 0)))) ""

            let e3 = Prod (Var, Prod (Var, Num 2))
            let printedResult = (deriv e3).ToString()
            Expect.equal printedResult "x*(x*0+2*1)+x*2*1" ""
        
            Expect.equal ((simpDeriv e3).ToString()) "x*2+x*2" ""

    [<Tests>]
    let tests =
        testList "Expression Simplification" [
            test01
        ]