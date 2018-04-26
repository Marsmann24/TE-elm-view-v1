module Savedview exposing (view)

import Model exposing (..)
import Topic exposing (Topic)
import Request

import Html exposing (Html, text)
import Material.Options exposing (cs, css, div, span, onClick, center)
import Material.Icon as Icon
import Material.Elevation as Elevation
import Material.Chip as Chip


view : Model -> String-> Html Msg
view model flex =
    div
        [ css "flex" flex
        , css "height" "100%"
        , css "margin" "3px 0px"
        , cs "flex__row"
        , Elevation.e6
        , primaryColor
        ]
        [ div
            [ css "height" "100%"]
            [ text "More Slots:"
            , div
                [ css "overflow" "auto"
                , css "height" "calc(100% - 20px)"
                , css "width" "250px"
                ]
                (List.indexedMap slotView2Chip model.slots.left)
            --(List.map (currentTopic2Chip model) model.currentTopics))
            ]
        , div
            [ css "height" "100%"]
            [ text "More Slots:"
            , div
                [ css "overflow" "auto"
                , css "height" "calc(100% - 20px)"
                , css "width" "250px"
                ]
                (List.indexedMap slotView2Chip model.slots.right)
            ]
        ]

slotView2Chip : Int -> View -> Html Msg
slotView2Chip id view =
    case view of
        TopicsView name _ _ _ ->
            Chip.chip Html.div
                [ css "width" "200px"
                , css "display" "block"
                ]
                [ Chip.content
                    [ center]
                    [ Icon.i "bubble_chart"
                    , span
                        [ css "width" "calc(100% - 48px)"
                        , onClick
                            (SlotToLastFromOther id)
                        ]
                        [ text name]
                    , Icon.view "cancel"
                        [ onClick
                            (RemoveSlotFromOther id)
                        ]
                    ]
                ]
        TermsView name _ _ ->
            Chip.chip Html.div
                [ css "width" "200px"
                , css "display" "block"
                ]
                [ Chip.content
                    [ center]
                    [ Icon.i "list"
                    , span
                        [ css "width" "calc(100% - 48px)"
                        , onClick
                            (SlotToLastFromOther id)
                        ]
                        [ text name]
                    , Icon.view "cancel"
                        [ onClick
                            (RemoveSlotFromOther id)
                        ]
                    ]
                ]
        DocumentsView name _ _ ->
            Chip.chip Html.div
                [ css "width" "200px"
                , css "display" "block"
                ]
                [ Chip.content
                    [ center]
                    [ Icon.i "art_track"
                    , span
                        [ css "width" "calc(100% - 48px)"
                        , onClick
                            (SlotToLastFromOther id)
                        ]
                        [ text name]
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
            , span
                [ onClick
                    (ExecCmd (Request.loadTerms topic 30 2))
                    --(ShowTerms topic.words)
                ]
                [ Icon.i "list"]
            , span
                [ onClick
                    (ExecCmd (Request.loadBestDocs topic Nothing "relevance" 2))
                    --(ShowDocuments
                    --    (List.filter
                    --        (Document.topicInDocument topic)
                    --        model.docs
                    --    ))
                ]
                [ Icon.i "art_track"]
            , Icon.view "cancel"
                [ onClick
                    (RemoveTopic topic.id)
                ]
            ]
        ]
