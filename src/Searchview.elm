module Searchview exposing (view)

import Model exposing (..)

import Html exposing (Html, text)
import Material.Options exposing (div, css, center, onClick)
import Material.List as Lists

view : Model -> String -> Html Msg
view model flex =
    div
        [ css "flex" flex
        , center
        ]
        [ Lists.ul
            []
            (List.map string2ListItem model.result)
        ]

string2ListItem : String -> Html Msg
string2ListItem string =
    Lists.li
        []
        [ Lists.content
            [ onClick (Found Empty)]
            [ text string]
        ]
