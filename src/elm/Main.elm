module Main exposing (..)

import Dict
import Evaluator.Helpers exposing (State)
import Evaluator.SmallStepEvaluator exposing (eval)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json
import Parser.SimpleParser exposing (parse)
import SimpleAST exposing (Env, Expr(..))
import Visualizer.Node as Node


type Msg
    = NoOp
    | UpdateString String
    | ParseString
    | KeyDown Int
    | EvalOneStep


type alias Model =
    { --ast : Maybe Expr
      evalStates : Maybe (List State)
    , finalState : Maybe State
    , currentAST : Maybe Expr
    , currentEnv : Maybe Env
    , textInput : Maybe String
    , reachedFinalStep : Bool
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }


model : Model
model =
    { evalStates = Nothing
    , finalState = Nothing
    , currentAST = Nothing
    , currentEnv = Nothing
    , textInput = Nothing
    , reachedFinalStep = True
    }


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ viewContent model ]


viewContent : Model -> Html Msg
viewContent model =
    div [ class "content" ]
        [ div [ class "top-container" ]
            [ [ textInput
              , button [ class "button btn", onClick ParseString ] [ text "Parse" ]
              , button [ class "button btn", onClick EvalOneStep ] [ text "Next" ]
              ]
                |> div [ class "input-container" ]
            , [ viewEval model.finalState ]
                |> h3 [ style [ ( "color", "white" ) ] ]
            , tempViewASTList model.evalStates
            ]
        , [ Node.drawTree model.currentAST ]
            |> div [ class "tree-container" ]
        ]


tempViewASTList stateList =
    case stateList of
        Nothing ->
            text "..."

        Just list ->
            let
                asts =
                    List.map Tuple.second list
            in
            text <| toString asts


viewEval : Maybe State -> Html Msg
viewEval mState =
    let
        title =
            "Result of evaluation: "
    in
    case mState of
        Just ( _, ast ) ->
            let
                stateList =
                    eval ast

                firstElement =
                    List.head stateList
            in
            case firstElement of
                Just ( _, val ) ->
                    text <| title ++ toString val

                _ ->
                    text <| title ++ "..."

        Nothing ->
            text <| title ++ "..."


textInput : Html Msg
textInput =
    input
        [ class "input"
        , placeholder "Skriv inn et uttrykk"
        , onInput UpdateString
        , onKeyDown KeyDown
        ]
        []


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

        KeyDown key ->
            if key == 13 then
                parseString model
            else
                model

        EvalOneStep ->
            -- TODO go to next state
            nextState model


nextState : Model -> Model
nextState model =
    case model.evalStates of
        Nothing ->
            model

        Just [] ->
            { model | reachedFinalStep = True }

        Just (( env, ast ) :: rest) ->
            { model | currentAST = Just ast, currentEnv = Just env, evalStates = Just rest }


parseString : Model -> Model
parseString model =
    case model.textInput of
        Nothing ->
            model

        Just str ->
            let
                newAST =
                    parse str

                states =
                    eval newAST

                mFinalState =
                    List.head states

                statesReversed =
                    List.reverse states
            in
            { model | currentAST = Just newAST, currentEnv = Just Dict.empty, evalStates = Just statesReversed, finalState = mFinalState }



-- HELPERS


astToString : Maybe Expr -> String
astToString mAST =
    case mAST of
        Just ast ->
            toString ast

        Nothing ->
            "..."


onKeyDown : (Int -> Msg) -> Attribute Msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)
