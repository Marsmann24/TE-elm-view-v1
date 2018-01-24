module Savedview exposing (view)

import Model exposing (..)
import Topic exposing (Topic)
import Request

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
            [ text (toString topic.id)
            , Icon.view "list"
                [ onClick
                    (ExecCmd (Request.loadTerms topic.id 30))
                    --(ShowTerms topic.words)
                ]
            , Icon.view "art_track"
                [ onClick
                    (ExecCmd (Request.loadBestDocs topic.id -1 "relevance"))
                    --(ShowDocuments
                    --    (List.filter
                    --        (Document.topicInDocument topic)
                    --        model.docs
                    --    ))
                ]
            , Icon.view "cancel"
                [ onClick
                    (RemoveTopic topic.id)
                ]
            ]
        ]
