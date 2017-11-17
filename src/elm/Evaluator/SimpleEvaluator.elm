module Evaluator.SimpleEvaluator exposing (eval)

import Dict
import Parser.SimpleParser exposing (parse)
import SimpleAST exposing (..)


eval : String -> Int
eval str =
    let
        expr =
            parse str
    in
    case expr of
        Error str ->
            Debug.log ("Cannot evaluate - " ++ str) -1

        _ ->
            case evalExpr Dict.empty expr of
                ( _, Num num ) ->
                    num

                ( _, a ) ->
                    Debug.log ("evaluated expression must return Int in the end, got this: " ++ toString a) -1


evalExpr : Env -> Expr -> ( Env, Expr )
evalExpr env expr =
    case expr of
        Num num ->
            ( env, Num num )

        Var str ->
            case Dict.get str env of
                Just expr ->
                    ( env, expr )

                Nothing ->
                    Error ("Variable " ++ str ++ " not defined in env: " ++ toString env)
                        |> (,) env

        Neg e ->
            let
                ( _, val ) =
                    evalExpr env e
            in
            case val of
                Num num ->
                    -num
                        |> Num
                        |> (,) env

                other ->
                    (Error <| "Cannot negate something other than int: " ++ toString other)
                        |> (,) env

        Add e1 e2 ->
            evalBinOp e1 e2 env (+)
                |> (,) env

        Mul e1 e2 ->
            evalBinOp e1 e2 env (*)
                |> (,) env

        Sub e1 e2 ->
            evalBinOp e1 e2 env (-)
                |> (,) env

        LessThan e1 e2 ->
            evalBinOp e1
                e2
                env
                (\v1 v2 ->
                    if v1 < v2 then
                        1
                    else
                        0
                )
                |> (,) env

        If eBool eThen eElse ->
            let
                ( _, vBool ) =
                    evalExpr env eBool

                ( _, vThen ) =
                    evalExpr env eThen

                ( _, vElse ) =
                    evalExpr env eElse
            in
            case vBool of
                Num bool ->
                    if typeOf vThen /= typeOf vElse then
                        (Error <|
                            "Type of then and else branch in if must be the same: "
                                ++ toString (typeOf vThen)
                                ++ " : "
                                ++ toString vThen
                                ++ ", "
                                ++ toString (typeOf vElse)
                                ++ toString vElse
                        )
                            |> (,) env
                    else if bool /= 0 then
                        vThen
                            |> (,) env
                    else
                        vElse
                            |> (,) env

                other ->
                    (Error <|
                        "Type of condition in if must be Num, but was: "
                            ++ toString (typeOf other)
                            ++ " : "
                            ++ toString other
                    )
                        |> (,) env

        SetVar str expr ->
            let
                ( _, val ) =
                    evalExpr env expr

                newEnv =
                    Dict.insert str val env
            in
            ( newEnv, val )

        SetFun funName argNames body ->
            let
                fun =
                    Fun argNames body env

                newEnv =
                    Dict.insert funName fun env
            in
            ( newEnv, fun )

        Fun argNames body localEnv ->
            Fun argNames body localEnv
                |> (,) env

        Apply funName args ->
            case Dict.get funName env of
                Nothing ->
                    (Error <| "Function " ++ funName ++ " doesnt exist in env: " ++ toString env)
                        |> (,) env

                Just (Fun argNames body localEnv) ->
                    let
                        argVals =
                            List.map (\arg -> Tuple.second <| evalExpr env arg) args

                        namesAndVals =
                            List.map2 (,) argNames argVals

                        newLocalEnv =
                            List.foldl (\( name, val ) newEnv -> Dict.insert name val newEnv) localEnv namesAndVals
                    in
                    evalExpr newLocalEnv body

                Just a ->
                    (Error <| "Only functions can be applied to things, this was: " ++ toString a)
                        |> (,) env

        Seq exprList ->
            List.foldl (\expr ( newEnv, _ ) -> evalExpr newEnv expr) ( env, Num -1 ) exprList

        Error str ->
            ( env, Error str )


evalBinOp : Expr -> Expr -> Env -> (Int -> Int -> Int) -> Expr
evalBinOp e1 e2 env op =
    case ( evalExpr env e1, evalExpr env e2 ) of
        ( ( _, Num num1 ), ( _, Num num2 ) ) ->
            Num <| op num1 num2

        ( ( _, other1 ), ( _, other2 ) ) ->
            Error <| "Both expressions in " ++ toString op ++ "-expression must be int: " ++ toString other1 ++ ", " ++ toString other2


typeOf : Expr -> Type
typeOf e =
    case e of
        Num _ ->
            TNum

        _ ->
            TFun
