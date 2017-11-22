module Topicsview exposing (view)

import Model exposing (..)

import Html exposing (Html, text)
import Html.Attributes exposing (style, class)
import Material.Options exposing (css, div, span, onClick)
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.Chip as Chip


view : Model -> String -> Int -> Html Msg
view model flex slotId =
    div [ Elevation.e6
        , css "flex" flex
        , css "margin" "3px 0px"
        ]
        (List.append
            [ div
                [ css "height" "45px"
                ]
                [ Button.render Mdl [0] model.mdl
                    [ Button.fab
                    , Button.minifab
                    , Button.raised
                    , Button.ripple
                    , onClick (HideTopics slotId)
                    , css "margin" "2px 4px"
                    , css "float" "right"
                    ]
                    [ Icon.i "close" ]
                ]
            ]
            (List.map topic2Chip model.topics))

topic2Chip : Topic -> Html Msg
topic2Chip topic =
    Chip.button
        [ css "width" "150px"
        , css "margin" "6px 2px"
        , onClick (ShowWordList topic.words)
        ]
        [ Chip.content []
            [ text topic.topicName]
        ]
