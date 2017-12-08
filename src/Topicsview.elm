module Topicsview exposing (view)

import Model exposing (..)

import Html exposing (Html, text)
import Html.Attributes exposing (style, class)
import Material.Options exposing (css, cs, div, span, onClick, center)
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
            [ Icon.view "bubble_chart" [ css "margin" "5px"]
            , Button.render Mdl [slotId] model.mdl
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
            (List.map (topic2Chip model) model.topics)
        ]

topic2Chip : Model -> Topic -> Html Msg
topic2Chip model topic =
    Chip.span
        [ css "width" "calc(100% - 40px)"
        , css "margin" "6px 4px"
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
            ]
        ]
