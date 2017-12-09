module Frontend.Model exposing (..)

import Backend.Parser.AST exposing (Env, Expr(..), State)


type alias Model =
    { currentAST : Maybe Expr
    , currentEnv : Maybe Env
    , nextStates : Maybe (List State)
    , previousStates : Maybe (List State)
    , finalResult : Maybe Expr
    , textInput : Maybe String
    }


model : Model
model =
    { currentAST = Nothing
    , currentEnv = Nothing
    , nextStates = Nothing
    , previousStates = Nothing
    , finalResult = Nothing
    , textInput = Nothing
    }
