module Main exposing (..)

import Frontend.Model as Model exposing (Model)
import Frontend.Update as Update
import Frontend.View as View
import Html


main : Program Never Model Update.Msg
main =
    Html.beginnerProgram { model = Model.model, view = View.view, update = Update.update }
