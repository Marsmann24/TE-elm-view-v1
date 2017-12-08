module Savedview exposing (view)

import Model exposing (..)

import Html exposing (Html, text)
import Material.Options exposing (css, div, span, onClick, center)
import Material.Color as Color
import Material.Icon as Icon
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
            (List.map (currentTopic2Chip model) model.currentTopics))

currentTopic2Chip : Model -> Topic -> Html Msg
currentTopic2Chip model topic =
    Chip.span
        [ if model.settings.bottom
            then
                css "width" "calc(100% - 40px)"
            else
                css "width" "200px"
        , center
        ]
        [ Chip.content
            [ center]
            [ text topic.topicName
            , Icon.view "list"
                [ onClick
                    (ShowWordList topic.words)
                ]
            , Icon.view "art_track"
                [ onClick
                    (ShowArticles
                        (List.filter
                            (topicInArticle topic)
                            model.articles
                        )
                    )
                ]
            , Icon.view "cancel"
                [ onClick
                    (RemoveTopic topic.topicID)
                ]
            ]
        ]
