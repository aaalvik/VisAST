module StackHelper exposing (..)

import SimpleAST exposing (..)
import Stack exposing (Stack)


type alias ExprStack =
    Stack Expr


type alias OpStack =
    Stack Op


type Op
    = BinOp (Expr -> Expr -> Expr)
    | UnOp (Expr -> Expr)
    | IfOp (Expr -> Expr -> Expr)
    | SetOp (Expr -> Expr)


popBottom : ExprStack -> ( Maybe Expr, ExprStack )
popBottom stack =
    let
        stackList =
            Stack.toList stack

        ( _, mLast, init ) =
            List.foldr
                (\x ( isLast, last, list ) ->
                    if isLast then
                        ( False, Just x, list )
                    else
                        ( False, last, x :: list )
                )
                ( True, Nothing, [] )
                stackList
    in
    ( mLast, Stack.listToStack init )
