module Backend.Evaluator.Helpers exposing (..)

import Backend.Parser.AST exposing (..)


isVal : Expr -> Bool
isVal expr =
    case expr of
        Num _ ->
            True

        {- Unsure about this, needed for lambda application -}
        Var _ ->
            True

        Fun _ _ _ ->
            True

        Lambda _ _ ->
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
