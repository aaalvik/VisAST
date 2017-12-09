module Frontend.Helpers.ToString exposing (..)

import Backend.Parser.AST exposing (..)
import Dict


showEnv : Env -> List String
showEnv env =
    let
        envList =
            List.reverse <| Dict.toList env

        stringList =
            List.map (uncurry showEnvElement) envList
    in
    stringList


showEnvElement : String -> Expr -> String
showEnvElement name body =
    name ++ " : " ++ toString body


showAST : Maybe Expr -> String
showAST mAST =
    case mAST of
        Just ast ->
            toString ast

        Nothing ->
            "..."
