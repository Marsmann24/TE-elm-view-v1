module Topicsview exposing (view)

import Model exposing (..)

import Html exposing (Html, text)
import Material.Options exposing (css, div, onClick)
import Material.Elevation as Elevation
import Material.Chip as Chip


view : Model -> Html Msg
view model =
    div [ Elevation.e6
        , css "display" "flex"
        , css "display" "-ms-flex"
        , css "display" "-webkit-flex"
        , css "flex-direction" "row"
        , css "flex" "1 1 25%"
        , css "margin" "3px 0px"
        ]
        [ div
            [ css "border-right" "1px solid grey"
            , css "flex" "1 1 20%"
            ]
            (List.append
                [ text "Platzhalter für ausgewählte Topics"]
                (List.map currentTopic2Chip model.currentTopics))
        , div
            [ css "flex" "4 3 80%"
            ]
            (List.map topic2Chip model.topics)
        ]

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

currentTopic2Chip : Topic -> Html Msg
currentTopic2Chip topic =
    Chip.button
        [ css "width" "calc(100% - 20px)"
        , css "margin" "6px 10px"
        , onClick (ShowWordList topic.words)
        , Chip.deleteIcon "cancel"
        , Chip.deleteClick (RemoveTopic topic.topicID)
        ]
        [ Chip.content []
            [ text topic.topicName]
        ]
