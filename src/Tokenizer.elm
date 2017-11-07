module Tokenizer exposing (tokenize)

import Char


tokenize : List Char -> List String
tokenize charList =
    case charList of
        ' ' :: rest ->
            tokenize rest

        '(' :: rest ->
            "(" :: tokenize rest

        ')' :: rest ->
            ")" :: tokenize rest

        '=' :: rest ->
            tokenize rest

        's' :: 'e' :: 't' :: rest ->
            "set" :: tokenize rest

        'i' :: 'f' :: rest ->
            "if" :: tokenize rest

        't' :: 'h' :: 'e' :: 'n' :: rest ->
            "then" :: tokenize rest

        'e' :: 'l' :: 's' :: 'e' :: rest ->
            tokenize rest

        '*' :: rest ->
            "*" :: tokenize rest

        '+' :: rest ->
            "+" :: tokenize rest

        '-' :: rest ->
            "-" :: tokenize rest

        '<' :: rest ->
            "<" :: tokenize rest

        ',' :: rest ->
            tokenize rest

        '\\' :: rest ->
            let
                ( lamVar, rest1 ) =
                    span (\c -> Char.isLower c || Char.isUpper c) (removeWs rest)
            in
            case removeWs rest1 of
                '-' :: '>' :: rest2 ->
                    "\\" :: String.fromList lamVar :: "->" :: tokenize rest2

                _ ->
                    Debug.log ("ERROR: Lambda backslash without arrow: " ++ String.fromList rest1) []

        (c :: rest) as str ->
            if Char.isDigit c then
                let
                    ( num, rest1 ) =
                        span Char.isDigit str
                in
                String.fromList num :: tokenize rest1
            else if Char.isLower c then
                let
                    ( var, rest1 ) =
                        span (\ch -> Char.isUpper ch || Char.isLower ch) str
                in
                String.fromList var :: tokenize rest1
            else
                Debug.log ("Input neither numeric nor string: " ++ String.fromList str) []

        [] ->
            []



-- HELPERS


removeWs : List Char -> List Char
removeWs list =
    case list of
        ' ' :: rest ->
            removeWs rest

        _ ->
            list


span : (Char -> Bool) -> List Char -> ( List Char, List Char )
span p xs =
    ( takeWhile p xs, dropWhile p xs )


takeWhile : (Char -> Bool) -> List Char -> List Char
takeWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                x :: takeWhile predicate xs
            else
                []


dropWhile : (Char -> Bool) -> List Char -> List Char
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                dropWhile predicate xs
            else
                list
