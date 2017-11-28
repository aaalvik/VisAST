module Evaluator.SmallStepEvaluator exposing (eval)

import Dict
import ListHelpers exposing (span)
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


eval : Expr -> List State
eval expr =
    let
        states =
            evalExpr Dict.empty expr [ ( Dict.empty, expr ) ]
    in
    case expr of
        Error str ->
            Debug.log ("Cannot evaluate - " ++ str) []

        _ ->
            case List.head states of
                Just ( _, Num num ) ->
                    --List.reverse states
                    states

                Just ( _, a ) ->
                    Debug.log ("evaluated expression must return Int in the end, got this: " ++ toString a) []

                Nothing ->
                    Debug.log "Evaluation resulted in empty list" []


evalExpr : Env -> Expr -> List State -> List State
evalExpr env expr prevStates =
    let
        nextStep =
            stepOne env expr
    in
    case nextStep of
        ( env, Num num ) ->
            nextStep :: prevStates

        ( env, Seq ((Error str) :: rest) ) ->
            Debug.log ("Hit an error node inside Seq: " ++ str) prevStates

        ( env, Error str ) ->
            Debug.log ("Hit an error node: " ++ str) prevStates

        ( newEnv, newExpr ) as state ->
            Debug.log ("Stepping one more time, expr was: " ++ toString newExpr) evalExpr newEnv newExpr (state :: prevStates)


stepOne : Env -> Expr -> State
stepOne env expr =
    case expr of
        Num num ->
            ( env, expr )

        Fun argNames body localEnv ->
            Fun argNames body localEnv
                |> (,) env

        Var str ->
            case Dict.get str env of
                Just expr ->
                    ( env, expr )

                Nothing ->
                    Error ("Variable " ++ str ++ " not defined in env: " ++ toString env)
                        |> (,) env

        Add e1 e2 ->
            evalBinOp e1 e2 env (+) Add
                |> (,) env

        Mul e1 e2 ->
            evalBinOp e1 e2 env (*) Mul
                |> (,) env

        Sub e1 e2 ->
            evalBinOp e1 e2 env (-) Sub
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
                LessThan
                |> (,) env

        If eBool eThen eElse ->
            case eBool of
                Num vBool ->
                    if vBool /= 0 then
                        ( env, eThen )
                    else
                        ( env, eElse )

                _ ->
                    let
                        ( _, nextEBool ) =
                            stepOne env eBool
                    in
                    ( env, If nextEBool eThen eElse )

        SetVar str expr ->
            case expr of
                (Num _) as val ->
                    let
                        newEnv =
                            Dict.insert str val env
                    in
                    ( newEnv, val )

                _ ->
                    let
                        ( _, nextExpr ) =
                            stepOne env expr
                    in
                    ( env, SetVar str nextExpr )

        SetFun funName argNames body ->
            let
                fun =
                    Fun argNames body env

                newEnv =
                    Dict.insert funName fun env
            in
            ( newEnv, fun )

        Apply funName args ->
            case Dict.get funName env of
                Nothing ->
                    (Error <| "Function " ++ funName ++ " doesnt exist in env: " ++ toString env)
                        |> (,) env

                Just (Fun argNames body localEnv) ->
                    if List.all isVal args then
                        let
                            namesAndVals =
                                List.map2 (,) argNames args

                            newLocalEnv =
                                List.foldl (\( name, val ) newEnv -> Dict.insert name val newEnv) localEnv namesAndVals
                        in
                        ( newLocalEnv, body )
                    else
                        let
                            nextArgVals =
                                evalArgsSmallStep env args
                        in
                        ( env, Apply funName nextArgVals )

                Just a ->
                    (Error <| "Only functions can be applied to things, this was: " ++ toString a)
                        |> (,) env

        Seq exprList ->
            case exprList of
                [ Num n ] ->
                    ( env, Num n )

                [ Error str ] ->
                    ( env, Error str )

                [ expr ] ->
                    let
                        ( newEnv, nextExpr ) =
                            stepOne env expr
                    in
                    ( newEnv, Seq [ nextExpr ] )

                -- throw away result, but keep env
                (Num n) :: notEmptyRest ->
                    stepOne env (Seq notEmptyRest)

                -- throw away result, but keep env
                (Fun _ _ _) :: notEmptyRest ->
                    stepOne env (Seq notEmptyRest)

                (Error str) :: rest ->
                    ( env, Error str )

                expr :: notEmptyRest ->
                    let
                        ( newEnv, newExpr ) =
                            stepOne env expr
                    in
                    ( newEnv, Seq <| newExpr :: notEmptyRest )

                [] ->
                    ( env, Error "List inside Seq node was empty" )

        Error str ->
            ( env, Error str )


evalArgsSmallStep : Env -> List Expr -> List Expr
evalArgsSmallStep env args =
    let
        ( vals, rest ) =
            span isVal args
    in
    case rest of
        expr :: rest1 ->
            let
                ( _, v ) =
                    stepOne env expr
            in
            vals ++ [ v ] ++ rest1

        [] ->
            vals


evalBinOp : Expr -> Expr -> Env -> (Int -> Int -> Int) -> (Expr -> Expr -> Expr) -> Expr
evalBinOp e1 e2 env op parentNode =
    case ( e1, e2 ) of
        ( Num num1, Num num2 ) ->
            Num <| op num1 num2

        ( (Num num) as leftChild, rightChild ) ->
            let
                ( _, newRightChild ) =
                    stepOne env rightChild
            in
            parentNode leftChild newRightChild

        ( Error err, _ ) ->
            Error <| "BinOp: Left child was error: " ++ err

        ( _, Error err ) ->
            Error <| "BinOp: Right child was error: " ++ err

        ( leftChild, _ ) ->
            let
                ( _, newLeftChild ) =
                    stepOne env leftChild
            in
            Debug.log ("Leftchild: " ++ toString newLeftChild ++ ", rightChild: " ++ toString e2) <|
                case newLeftChild of
                    Error err ->
                        Error "New left child was error"

                    _ ->
                        parentNode newLeftChild e2
