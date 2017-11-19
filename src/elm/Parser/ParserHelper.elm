module Parser.ParserHelper exposing (..)

import ListHelpers exposing (span)


type PrecedenceType
    = PMul
    | PAdd
    | PSub
    | PLessThan
    | PIf
    | PSet
    | PFunction
    | PVar
    | PNum
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

        "-" :: _ ->
            PSub

        "if" :: _ ->
            PIf

        "set" :: _ ->
            PSet

        "function" :: _ ->
            PFunction

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
        --PNeg -> 2
        -- PApply -> 3 TODO Fix
        PMul ->
            2

        PAdd ->
            3

        PSub ->
            3

        PLessThan ->
            4

        _ ->
            6


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
