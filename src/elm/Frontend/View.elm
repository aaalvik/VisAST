module Frontend.View exposing (..)

import Backend.Evaluator.SmallStepEvaluator as SmallStep
import Backend.Parser.AST exposing (..)
import Frontend.Helpers.ToString as ToString
import Frontend.Model exposing (Model)
import Frontend.Tree as Tree
import Frontend.Update exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json


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
        , viewAST model
        ]


viewAST : Model -> Html Msg
viewAST model =
    [ viewEnv model.currentEnv
        |> div [ class "env" ]
    , [ Tree.drawTree model.currentAST ]
        |> div [ class "tree-container" ]
    ]
        |> div
            [ class "ast-container" ]


viewEval : Maybe Expr -> Html Msg
viewEval mAst =
    let
        title =
            "Final result of \n evaluation: "
    in
    case mAst of
        Just ast ->
            let
                stateList =
                    SmallStep.eval ast

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


viewEnv : Maybe Env -> List (Html Msg)
viewEnv mEnv =
    case mEnv of
        Nothing ->
            []

        Just env ->
            div [] [ h4 [] [ text "Environment:" ] ] :: List.map (div [] << List.singleton << text) (ToString.showEnv env)


textInput : Html Msg
textInput =
    input
        [ class "input"
        , placeholder "Skriv inn et uttrykk"
        , onInput UpdateString
        , onKeyDown KeyDown
        ]
        []


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)
