module Frontend.Update exposing (..)

import Backend.Evaluator.SmallStepEvaluator as SmallStep
import Backend.Parser.Parser as Parser
import Dict
import Frontend.Model exposing (..)
import Frontend.Step.Step as Step


type Msg
    = NoOp
    | UpdateString String
    | ParseString
      --| KeyDown Int
    | NextState
    | PreviousState


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        UpdateString inp ->
            { model
                | textInput =
                    if String.isEmpty inp then
                        Nothing
                    else
                        Just inp
            }

        ParseString ->
            parseString model

        -- KeyDown key ->
        --     if key == 13 then
        --         parseString model
        --     else
        --         model
        NextState ->
            Step.nextState model

        PreviousState ->
            Step.previousState model


parseString : Model -> Model
parseString model =
    case model.textInput of
        Nothing ->
            model

        Just str ->
            let
                newAST =
                    Parser.parse str

                states =
                    SmallStep.eval newAST

                mFinalState =
                    List.head states

                statesReversed =
                    List.reverse states
            in
            { model
                | currentAST = Just newAST
                , currentEnv = Just Dict.empty
                , nextStates = Just statesReversed
                , previousStates = Just []
                , finalResult = Maybe.map Tuple.second mFinalState
            }
