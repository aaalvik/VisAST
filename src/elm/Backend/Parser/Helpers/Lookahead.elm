module Backend.Parser.Helpers.Lookahead exposing (..)

import Backend.Helpers.ListHelpers exposing (span)


type PrecedenceType
    = PMul
    | PAdd
    | PLast



-- HELPER FUNCTIONS
--highest precedence == 1, then increasing number for decreasing precedence


lookaheadOp : List String -> PrecedenceType
lookaheadOp list =
    case list of
        "(" :: rest ->
            lookaheadOp <| skipTil ")" rest

        "*" :: _ ->
            PMul

        "+" :: _ ->
            PAdd

        _ :: rest ->
            lookaheadOp rest

        [] ->
            PLast


hasHigherPrecedence : PrecedenceType -> PrecedenceType -> Bool
hasHigherPrecedence nextP curP =
    precedenceNumber nextP
        < precedenceNumber curP
        || nextP
        == PLast


precedenceNumber : PrecedenceType -> Int
precedenceNumber pType =
    case pType of
        PMul ->
            1

        PAdd ->
            2

        _ ->
            100


readTil : String -> List String -> ( List String, List String )
readTil stopAt l =
    let
        ( read, rest ) =
            span (\x -> x /= stopAt) l
    in
    case rest of
        x :: rest1 ->
            ( read, rest1 )

        [] ->
            ( read, [] )


skipTil : String -> List String -> List String
skipTil stopAt l =
    Tuple.second <| readTil stopAt l
