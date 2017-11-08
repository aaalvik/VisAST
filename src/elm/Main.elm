module Main exposing (..)

import Html exposing (..)
import SimpleEvaluator exposing (eval)


main =
    eval "function foo x = x + 1; foo (3)"
        |> toString
        |> text



--<| eval "function foo x y = x + y; foo (3,4);"
