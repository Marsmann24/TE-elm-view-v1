module Wordlistview exposing (view)

import Model exposing (..)

import Html exposing (Html, text)
import Html.Events
import Material.Options exposing (css, div, attribute, onClick)
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.List as Lists

view : Model -> String -> Int -> Html Msg
view model flex slotId =
    div
        [ css "flex" flex
        , css "margin" "3px 0px"
        , Elevation.e4
        ]
        [ div
            [ css "height" "45px"
            ]
            [ Button.render Mdl [0] model.mdl
                [ Button.fab
                , Button.minifab
                , Button.raised
                , Button.ripple
                , onClick (HideWordList slotId)
                , css "margin" "2px 4px"
                , css "float" "right"
                ]
                [ Icon.i "close" ]
            ]
        , Lists.ul
            []
            (List.map (topic2WordList model.currentWord) model.wordList)
        ]

topic2WordList : String -> String -> Html Msg
topic2WordList selected word =
    Lists.li
        []
        [ Lists.content
            (if (word == selected)
                then
                    [ Elevation.e0]
                else
                    [ attribute <| Html.Events.onClick (ShowArticles word)])
            [ text word]
        ]
