module Frontend.Step.Step exposing (..)

import Frontend.Model exposing (..)


nextState : Model -> Model
nextState model =
    case ( model.nextStates, model.currentEnv, model.currentAST ) of
        ( Just ((( env, ast ) as state) :: rest), Just curEnv, Just curAst ) ->
            let
                currentState =
                    ( curEnv, curAst )

                newPrevStates =
                    case model.previousStates of
                        Nothing ->
                            [ currentState ]

                        Just xs ->
                            currentState :: xs

                newNextStates =
                    rest
            in
            { model | currentAST = Just ast, currentEnv = Just env, nextStates = Just newNextStates, previousStates = Just newPrevStates }

        _ ->
            model


previousState : Model -> Model
previousState model =
    case ( model.previousStates, model.currentEnv, model.currentAST ) of
        ( Just (( env, ast ) :: rest), Just curEnv, Just curAst ) ->
            let
                currentState =
                    ( curEnv, curAst )

                newNextStates =
                    case model.nextStates of
                        Nothing ->
                            [ currentState ]

                        Just xs ->
                            currentState :: xs

                newPreviousStates =
                    rest
            in
            { model | currentAST = Just ast, currentEnv = Just env, nextStates = Just newNextStates, previousStates = Just newPreviousStates }

        _ ->
            model
