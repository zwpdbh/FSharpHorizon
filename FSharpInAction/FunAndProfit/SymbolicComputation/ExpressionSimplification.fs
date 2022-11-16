namespace FunAndProfit

module ExpressionSimplification = 
    open System


    //  Implementation of symbolic differentiation over this simple expression type.
    type Expr = 
        | Var 
        | Num of int 
        | Sum of Expr * Expr 
        | Prod of Expr * Expr 

    let rec deriv expr = 
        match expr with 
        | Var -> Num 1 
        | Num _ -> Num 0 
        | Sum (e1, e2) -> Sum (deriv e1, deriv e2)
        | Prod (e1, e2) -> Sum (Prod (e1, deriv e2), Prod (e2, deriv e1))