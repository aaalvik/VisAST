module ParserHelper exposing (..)


type PrecedenceType
    = PParens
      --| PNeg
      --| PApply
    | PMul
    | PAdd
    | PSub
    | PLessThan
    | PIf
      --| PLet
      --| PLetFun
      --| PLambda
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

        PLast ->
            1000

        _ ->
            6
