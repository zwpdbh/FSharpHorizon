namespace FunAndProfit

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