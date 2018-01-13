module Backend.Evaluator.SmallStepEvaluator exposing (eval)

import Backend.Evaluator.BigStepEvaluator as BigStep
import Backend.Evaluator.Helpers as Helpers
import Backend.Helpers.ListHelpers exposing (span)
import Backend.Parser.AST exposing (..)
import Dict


eval : Expr -> List State
eval expr =
    let
        states =
            evalExpr Dict.empty expr []
    in
    case expr of
        Error str ->
            Debug.log ("Cannot evaluate - " ++ str) []

        _ ->
            case List.head states of
                Just ( _, Num num ) ->
                    states

                Just ( _, Fun _ _ _ ) ->
                    states

                Just ( _, Lambda _ _ ) ->
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

        ( env, Fun _ _ _ ) ->
            nextStep :: prevStates

        ( env, Lambda _ _ ) ->
            nextStep :: prevStates

        ( env, Seq ((Error str) :: rest) ) ->
            Debug.log ("Hit an error node inside Seq: " ++ str) prevStates

        ( env, Error str ) ->
            Debug.log ("Hit an error node: " ++ str) prevStates

        ( newEnv, newExpr ) as state ->
            evalExpr newEnv newExpr (state :: prevStates)


stepOne : Env -> Expr -> State
stepOne env expr =
    case expr of
        Num num ->
            ( env, expr )

        Fun argNames body localEnv ->
            ( env, expr )

        Lambda lamStr body ->
            ( env, expr )

        Var str ->
            case Dict.get str env of
                Just expr ->
                    ( env, expr )

                Nothing ->
                    Error ("Variable " ++ str ++ " not defined in env: " ++ toString env)
                        |> (,) env

        Add e1 e2 ->
            evalBinOp e1 e2 env (+) Add

        Mul e1 e2 ->
            evalBinOp e1 e2 env (*) Mul

        Sub e1 e2 ->
            evalBinOp e1 e2 env (-) Sub

        LessThan e1 e2 ->
            evalEquation e1 e2 (<) env LessThan

        BiggerThan e1 e2 ->
            evalEquation e1 e2 (>) env BiggerThan

        Equal e1 e2 ->
            evalEquation e1 e2 (==) env Equal

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
                    if List.length argNames /= List.length args then
                        (Error <| "Function " ++ funName ++ " was applied to wrong number of arguments")
                            |> (,) env
                    else if List.all Helpers.isVal args then
                        BigStep.evalApp argNames body args localEnv
                    else
                        let
                            nextArgVals =
                                evalArgsSmallStep env args
                        in
                        ( env, Apply funName nextArgVals )

                Just ((Lambda _ _) as lambda) ->
                    let
                        arg =
                            case args of
                                a :: _ ->
                                    a

                                [] ->
                                    Error "Tried to apply an empty argument to a lambda"
                    in
                    stepOne env (ApplyLam lambda arg)

                Just a ->
                    (Error <| "Only functions can be applied to things, this was: " ++ toString a)
                        |> (,) env

        ApplyLam lambda arg ->
            case lambda of
                Lambda lamVar body ->
                    if Helpers.isVal arg then
                        let
                            ( _, val ) =
                                BigStep.evalApp [ lamVar ] body [ arg ] env
                        in
                        ( env, val )
                    else
                        let
                            ( _, newArg ) =
                                stepOne env arg
                        in
                        ( env, ApplyLam lambda newArg )

                _ ->
                    ( env, Error "Error: Lambda application must contain a lambda" )

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
                    Debug.log "Throwing away result of num, jumping to next expression in Seq" ( env, Seq notEmptyRest )

                -- throw away result, but keep env
                (Fun _ _ _) :: notEmptyRest ->
                    Debug.log "Throwing away result of fun, jumping to next expression in Seq" ( env, Seq notEmptyRest )

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
            span Helpers.isVal args
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


evalBinOp : Expr -> Expr -> Env -> (Int -> Int -> Int) -> (Expr -> Expr -> Expr) -> State
evalBinOp e1 e2 env op parentNode =
    case ( e1, e2 ) of
        ( Num num1, Num num2 ) ->
            ( env, Num <| op num1 num2 )

        ( (Num num) as leftChild, rightChild ) ->
            let
                ( newEnv, newRightChild ) =
                    stepOne env rightChild
            in
            ( newEnv, parentNode leftChild newRightChild )

        ( Error err, _ ) ->
            ( env, Error <| "BinOp: Left child was error: " ++ err )

        ( _, Error err ) ->
            ( env, Error <| "BinOp: Right child was error: " ++ err )

        ( leftChild, _ ) ->
            let
                ( newEnv, newLeftChild ) =
                    stepOne env leftChild
            in
            case newLeftChild of
                Error err ->
                    ( newEnv, Error "New left child was error" )

                _ ->
                    ( newEnv, parentNode newLeftChild e2 )


evalEquation : Expr -> Expr -> (Int -> Int -> Bool) -> Env -> (Expr -> Expr -> Expr) -> State
evalEquation e1 e2 op env node =
    evalBinOp e1
        e2
        env
        (\v1 v2 ->
            if op v1 v2 then
                1
            else
                0
        )
        node
