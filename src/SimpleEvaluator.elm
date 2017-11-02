module SimpleEvaluator exposing (eval)

import SimpleAST exposing (..)
import Dict exposing (..)


eval : Expr -> Int
eval expr =
    evalExpr emptyEnv expr


evalExpr : Env -> Expr -> Int
evalExpr env expr =
    case expr of
        InParens e ->
            evalExpr env e

        Neg e ->
            -(evalExpr env e)

        Add e1 e2 ->
            (evalExpr env e1) + (evalExpr env e2)

        Mul e1 e2 ->
            (evalExpr env e1) * (evalExpr env e2)

        Sub e1 e2 ->
            (evalExpr env e1) - (evalExpr env e2)

        LessThan e1 e2 ->
            let
                v1 =
                    evalExpr env e1

                v2 =
                    evalExpr env e2
            in
                if v1 < v2 then
                    1
                else
                    0

        If eBool eThen eElse ->
            let
                bool =
                    evalExpr env eBool

                vThen =
                    evalExpr env eThen

                vElse =
                    evalExpr env eElse
            in
                if bool /= 0 then
                    vThen
                else
                    vElse

        Let str e1 e2 ->
            let
                v1 =
                    evalExpr env e1

                newEnv =
                    { env | numEnv = Dict.insert str v1 env.numEnv }
            in
                evalExpr newEnv e2

        LetFun name argNames e1 e2 ->
            let
                newEnv =
                    { env | funEnv = Dict.insert name (Fun argNames e1 env) env.funEnv }
            in
                evalExpr newEnv e2

        --Lambda str body ->
        Apply funName args ->
            case Dict.get funName env.funEnv of
                Nothing ->
                    Debug.log ("Function " ++ funName ++ " doesnt exist in funEnv: " ++ toString env.funEnv) -1

                Just (Fun argNames body localEnv) ->
                    let
                        argVals =
                            List.map (\arg -> evalExpr env arg) args

                        newLocalEnv =
                            addArgumentsToEnv argNames argVals localEnv
                    in
                        evalExpr newLocalEnv body

        Var str ->
            case Dict.get str env.numEnv of
                Just val ->
                    val

                Nothing ->
                    Debug.log ("Variable " ++ str ++ " not defined in numEnv: " ++ toString env.numEnv) -1

        Num num ->
            num

        Error str ->
            Debug.log ("Hit an Error node: " ++ str) -1


addArgumentsToEnv : ArgNames -> List Int -> Env -> Env
addArgumentsToEnv argNames argVals env =
    let
        zipped =
            List.map2 (,) argNames argVals

        newNumEnv =
            List.foldl (\( argName, argVal ) tempNumEnv -> Dict.insert argName argVal tempNumEnv) env.numEnv zipped
    in
        { env | numEnv = newNumEnv }
