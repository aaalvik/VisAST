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
    | NextState
    | PreviousState


type alias Model =
    { currentAST : Maybe Expr
    , currentEnv : Maybe Env
    , nextStates : Maybe (List State)
    , previousStates : Maybe (List State)
    , finalResult : Maybe Expr
    , textInput : Maybe String
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }


model : Model
model =
    { currentAST = Nothing
    , currentEnv = Nothing
    , nextStates = Nothing
    , previousStates = Nothing
    , finalResult = Nothing
    , textInput = Nothing
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
              , button [ class "button btn", onClick PreviousState ] [ text "Previous" ]
              , button [ class "button btn", onClick NextState ] [ text "Next" ]
              ]
                |> div [ class "input-container" ]
            , [ viewEval model.finalResult ]
                |> h3 [ style [ ( "color", "white" ) ] ]
            ]
        , [ Node.drawTree model.currentAST ]
            |> div [ class "tree-container" ]
        ]


viewEval : Maybe Expr -> Html Msg
viewEval mAst =
    let
        title =
            "Final result of evaluation: "
    in
    case mAst of
        Just ast ->
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
                    text <| title ++ ""

        Nothing ->
            text <| title ++ ""



-- showEnv : Env -> String
-- showEnv env =


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

        NextState ->
            nextState model

        PreviousState ->
            previousState model


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
            { model
                | currentAST = Just newAST
                , currentEnv = Just Dict.empty
                , nextStates = Just statesReversed
                , previousStates = Just []
                , finalResult = Maybe.map Tuple.second mFinalState
            }



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
