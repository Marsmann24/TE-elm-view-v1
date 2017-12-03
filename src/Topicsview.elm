module Topicsview exposing (view)

import Model exposing (..)

import Html exposing (Html, text)
import Html.Attributes exposing (style, class)
import Material.Options exposing (css, cs, div, span, onClick)
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.Chip as Chip


view : Model -> String -> Int -> Html Msg
view model flex slotId =
    div [ cs "slot"
        , css "flex" flex
        , Elevation.e0
        , Color.background (Color.color Color.Grey Color.S500)
        ]
        [ div
            [ css "height" "45px"
            ]
            [ Button.render Mdl [slotId] model.mdl
                [ cs "slot__close_button"
                , Button.fab
                , Button.minifab
                , Button.raised
                , onClick (HideTopics slotId)
                ]
                [ Icon.i "close" ]
            ]
        , div
            [ cs "slot__content"
            ]
            (List.map topic2Chip model.topics)
        ]

topic2Chip : Topic -> Html Msg
topic2Chip topic =
    Chip.button
        [ css "width" "calc(100% - 4px)"
        , css "margin" "6px 2px"
        , onClick (ShowWordList topic.words)
        ]
        [ Chip.content []
            [ text topic.topicName]
        ]
