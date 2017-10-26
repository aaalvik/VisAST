module Main exposing (..)

import SimpleParser exposing (parse)
import Platform exposing (program)


nodeProgram : a -> Program Never () ()
nodeProgram _ =
    program
        { init = ( (), Cmd.none )
        , update = \() -> \() -> ( (), Cmd.none )
        , subscriptions = \() -> Sub.none
        }


main : Program Never () ()
main =
    nodeProgram (parse "Hei")
