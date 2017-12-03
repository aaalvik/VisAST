module Evaluator.Helpers exposing (..)

import SimpleAST exposing (..)


type alias State =
    ( Env, Expr )


isVal : Expr -> Bool
isVal expr =
    case expr of
        Num _ ->
            True

        Fun _ _ _ ->
            True

        _ ->
            False


isTrue : Expr -> Bool
isTrue expr =
    case expr of
        Num n ->
            n /= 0

        _ ->
            False


typeOf : Expr -> Type
typeOf e =
    case e of
        Num _ ->
            TNum

        _ ->
            TFun
