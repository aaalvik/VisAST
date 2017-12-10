module Backend.Parser.Helpers.Lookahead exposing (..)


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



{- Reads from list until hit stop-string. Ignores things inside parentheses -}


readTil : String -> List String -> ( List String, List String )
readTil stopAt l =
    let
        stop x =
            x == stopAt

        ( _, _, read ) =
            List.foldl
                (\x ( done, numOpened, seen ) ->
                    if done then
                        ( done, 0, seen )
                    else if stop x && numOpened == 0 then
                        ( True, 0, seen )
                    else if x == "(" then
                        ( done, numOpened + 1, seen ++ [ x ] )
                    else if x == ")" then
                        ( done, numOpened - 1, seen ++ [ x ] )
                    else
                        ( done, numOpened, seen ++ [ x ] )
                )
                ( False, 0, [] )
                l

        rest =
            List.drop (List.length read + 1) l
    in
    ( read, rest )


skip : String -> List String -> List String
skip skipStr xs =
    case xs of
        c :: rest ->
            if c == skipStr then
                rest
            else
                xs

        _ ->
            xs


skipTil : String -> List String -> List String
skipTil stopAt xs =
    Tuple.second <| readTil stopAt xs
