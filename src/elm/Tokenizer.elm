module Tokenizer exposing (tokenize)

import Char
import ListHelpers exposing (removeWs, span)


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
            "=" :: tokenize rest

        's' :: 'e' :: 't' :: ' ' :: rest ->
            "set" :: tokenize rest

        'f' :: 'u' :: 'n' :: 'c' :: 't' :: 'i' :: 'o' :: 'n' :: ' ' :: rest ->
            "function" :: tokenize rest

        'i' :: 'f' :: ' ' :: rest ->
            "if" :: tokenize rest

        't' :: 'h' :: 'e' :: 'n' :: ' ' :: rest ->
            "then" :: tokenize rest

        'e' :: 'l' :: 's' :: 'e' :: ' ' :: rest ->
            "else" :: tokenize rest

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
                        span isVariable str
                in
                String.fromList var :: tokenize rest1
            else
                Debug.log ("Input neither numeric nor string: " ++ String.fromList str) []

        [] ->
            []


isVariable : Char -> Bool
isVariable c =
    Char.isLower c || Char.isUpper c || Char.isDigit c || c == '_' || c == '-'
