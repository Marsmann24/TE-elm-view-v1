module TE_elm_v1 exposing (..)

--import Mainview_v1
import Mainview_v2
import Mainview_v3
import Mainview_v4
import Update
import Init

import Model exposing (Model, Msg)

import Html


main : Program Never Model Msg
main =
    Html.program
        { init = Init.init
        , update = Update.update
        , view = view
        , subscriptions = subscriptions
        }

view model =
    if model.settings.view2
        then
            Mainview_v2.view model
        else
            Mainview_v4.view model

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
