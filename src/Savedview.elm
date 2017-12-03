module Savedview exposing (view)

import Model exposing (..)

import Html exposing (Html, text)
import Html.Attributes exposing (style, class)
import Material.Options exposing (css, div, span, onClick)
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Chip as Chip


view : Model -> String-> Html Msg
view model flex =
    div
        [ css "flex" flex
        , css "margin" "3px 0px"
        , Elevation.e6
        , Color.background (Color.color Color.Grey Color.S500)
        ]
        (List.append
            [ ]
            (List.map (currentTopic2Chip model.settings) model.currentTopics))

currentTopic2Chip : Settings -> Topic -> Html Msg
currentTopic2Chip settings topic =
    Html.div
        [ class "mdl-chip"
        , style
            [ if settings.bottom
                then
                    ("width", "calc(100% - 40px)")
                else
                    ("width", "200px")
            , ("margin", "6px 10px")
            ]
        ]
        [ Chip.button
            [ css "margin" "0"
            , css "float" "left"
            , css "width" "calc(100% - 30px)"
            , onClick (ShowWordList topic.words)
            ]
            [ Chip.content []
                [ text topic.topicName]
            ]
        , Chip.button
            [ css "margin" "0"
            , css "padding" "0"
            , css "float" "right"
            , Chip.deleteIcon "cancel"
            , Chip.deleteClick (RemoveTopic topic.topicID)
            ]
            []
        ]
