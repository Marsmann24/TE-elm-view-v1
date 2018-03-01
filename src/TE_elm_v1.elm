module TE_elm_v1 exposing (..)

--import Mainview_v1
import Mainview_v2
import Update
import Init

import Model exposing (Model, Msg)

import Html


main : Program Never Model Msg
main =
    Html.program
        { init = Init.init
        , update = Update.update
        , view = Mainview_v2.view
        , subscriptions = subscriptions
        }

--view model =
    --if model.settings.view2
    --    then
    --        Mainview_v2.view model
    --    else
    --        Mainview_v1.view model

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
