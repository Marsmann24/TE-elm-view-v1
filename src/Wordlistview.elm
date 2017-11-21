module Wordlistview exposing (view)

import Model exposing (..)

import Html exposing (Html, text)
import Material.Options exposing (css, div, onClick)
import Material.Elevation as Elevation
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
            , css "border-bottom" "1px solid #e0e0e0"
            ]
            [ Button.render Mdl [0] model.mdl
                [ Button.raised
                , Button.ripple
                , onClick (HideWordList slotId)
                ]
                [ text "Hide"]
            ]
            , Lists.ul
                []
                (List.map topic2WordList model.wordList)
        ]

topic2WordList : String -> Html Msg
topic2WordList word =
    Lists.li
        []
        [ Lists.content [] [ text word]]
