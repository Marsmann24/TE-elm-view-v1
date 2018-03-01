module Savedview exposing (view)

import Model exposing (..)
import Topic exposing (Topic)
import Request

import Html exposing (Html, text)
import Material.Options exposing (css, div, span, onClick, center)
import Material.Icon as Icon
import Material.Elevation as Elevation
import Material.Chip as Chip


view : Model -> String-> Html Msg
view model flex =
    div
        [ css "flex" flex
        , css "margin" "3px 0px"
        , Elevation.e6
        , primaryColor
        ]
        [ div []
            [ text "More Slots:"
            ]
        , div []
            (List.indexedMap slotView2Chip model.slots.more)
            --(List.map (currentTopic2Chip model) model.currentTopics))
        ]

slotView2Chip : Int -> View -> Html Msg
slotView2Chip id view =
    case view of
        TopicsView name _ _ ->
            Chip.chip Html.div
                [ css "width" "200px"
                , css "display" "grid"
                ]
                [ Chip.content
                    [ center]
                    [ span
                        [ css "width" "160px"
                        , onClick
                            (SlotToLastFromOther id)
                        ]
                        [ Icon.i "bubble_chart"
                        , text name
                        ]
                    , Icon.view "cancel"
                        [ onClick
                            (RemoveSlotFromOther id)
                        ]
                    ]
                ]
        TermsView name _ ->
            Chip.chip Html.div
                [ css "width" "200px"
                , css "display" "grid"
                ]
                [ Chip.content
                    [ center]
                    [ span
                        [ css "width" "160px"
                        , onClick
                            (SlotToLastFromOther id)
                        ]
                        [ Icon.i "list"
                        , text name
                        ]
                    , Icon.view "cancel"
                        [ onClick
                            (RemoveSlotFromOther id)
                        ]
                    ]
                ]
        DocumentsView name _ ->
            Chip.chip Html.div
                [ css "width" "200px"
                , css "display" "grid"
                ]
                [ Chip.content
                    [ center]
                    [ span
                        [ css "width" "160px"
                        , onClick
                            (SlotToLastFromOther id)
                        ]
                        [ Icon.i "art_track"
                        , text name
                        ]
                    , Icon.view "cancel"
                        [ onClick
                            (RemoveSlotFromOther id)
                        ]
                    ]
                ]
        _ ->
            div [] []

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
