module Parser.ParserHelper exposing (..)


type PrecedenceType
    = PParens
    | PMul
    | PAdd
    | PSub
    | PLessThan
    | PIf
    | PSet
    | PVar
    | PNum
    | PLast



-- HELPER FUNCTIONS
--highest precedence == 1, then increasing number for decreasing precedence


lookaheadOp : List String -> PrecedenceType
lookaheadOp list =
    case list of
        "(" :: _ ->
            PParens

        ")" :: _ ->
            PParens

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

        _ :: rest ->
            lookaheadOp rest

        [] ->
            PLast


hasHigherPrecedence : PrecedenceType -> PrecedenceType -> Bool
hasHigherPrecedence p1 p2 =
    precedenceNumber p1 <= precedenceNumber p2


precedenceNumber : PrecedenceType -> Int
precedenceNumber pType =
    case pType of
        PParens ->
            1

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

        PIf ->
            5

        PSet ->
            6

        PLast ->
            1000

        _ ->
            6
