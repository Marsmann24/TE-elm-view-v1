module Topicsview exposing (view)

import Model exposing (..)
import Topic exposing (..)
import Document
import Request

import Html exposing (Html, text)
import Html.Attributes exposing (style, class)
import Material.Options exposing (css, cs, div, span, onClick, center)
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.Chip as Chip
import ContainerCache exposing (Page(..))

view : Model -> String -> Int -> String -> Html Msg
view model flex slotId slotName =
    div [ cs "slot"
        , if (slotId == model.settings.slotToDelete)
            then cs "slot__remove"
            else css "flex" flex
        , Elevation.e0
        , primaryColor
        ]
        [ div
            [ css "height" "45px"
            , css "text-align" "center"
            ]
            [ Icon.view "bubble_chart"
                [ css "margin" "5px"
                , css "text-align" "left"
                ]
            , text slotName
            , Button.render Mdl [slotId] model.mdl
                [ cs "slot__close_button"
                , Button.fab
                , Button.minifab
                , Button.raised
                , onClick (DeleteSlot slotId (HideTopics slotId))
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
            [ css "width" "100%"
            , center
            ]
            [ span
                [ css "width" "calc(100% - 48px)"
                , css "text-align" "center"
                ]
                [ text (toString topic.id)]
            , Icon.view "list"
                [ onClick
                    (ExecCmd (Request.loadTerms topic.id 30))
                    --(ShowTermList topic.top_terms)
                ]
            , Icon.view "art_track"
                [ onClick
                    (ExecCmd (Request.loadBestDocs topic.id -1 "relevance"))
                    --ShowDocuments
                    --    (List.filter
                    --        (Document.topicInDoc topic)
                    --        model.docs
                    --    ))
                ]
            ]
        ]
