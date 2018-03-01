module Documentsview exposing (view)

import Model exposing (..)
import Document exposing (..)
import Topic
import Request

import Html exposing (Html, text)
import Material.Options exposing (css, cs, div, span, onClick, onMouseEnter, onMouseLeave, onMouseDown, onMouseUp)
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.Color as Color
import Material.Card as Card

view : Model -> String -> Int -> String -> Html Msg
view model flex slotId slotName =
    div
        [ cs "slot"
        , if (slotId == model.settings.slotToDelete)
            then cs "slot__remove"
            else css "flex" flex
        , Elevation.e0
        , primaryColor
        ]
        [ div
            [ css "height" "45px"
            ]
            [ Icon.view "art_track" [ css "margin" "5px"]
            , text slotName
            , Button.render Mdl [slotId] model.mdl
                [ cs "slot__close_button"
                , Button.fab
                , Button.minifab
                , Button.raised
                , Button.ripple
                , onClick (DeleteSlot slotId (HideTerms slotId))
                ]
                [ Icon.i "close" ]
            ]
        , div
            [ cs "slot__content"
            ]
            (List.map2 (doc2CardView model) model.docs (List.range 1 (List.length model.docs)))
        ]

doc2CardView : Model -> Doc -> Int -> Html Msg
doc2CardView model doc cardID =
    Card.view
        [ css "height" "100px"
        , css "width" "94%"
        , css "margin" "4% 3%"
        , Color.background (Color.color Color.Brown Color.S500)
        , if (cardID == model.currentDocument.cardID)
            then Elevation.e0
          else if (cardID == model.raised)
            then Elevation.e16
          else Elevation.e8
        , Elevation.transition 250
        , onMouseEnter (Raise cardID)
        , onMouseLeave (Raise -1)
        --, onClick (ChangeCurrentDoc cardID doc)
        , onMouseUp (ExecuteActionIfNone (ExecCmd (Request.loadDoc doc.document_id)))
        ]
        [ Card.title
            [ css "padding" "4px"
            ]
            [ Card.head
                [ Color.text Color.white
                , css "font-size" "14px"
                ]
                [ span
                    [ css "width" "calc(100% - 48px)!important"]
                    [ text doc.title]
                , Icon.view "bubble_chart"
                    [ css "float" "right"
                    , onMouseDown
                        (SelectAction
                            (ShowTopics
                                (List.filterMap
                                    (Topic.topicId2Topic model.topics)
                                    doc.top_topic
                                )
                            )
                        )
                    ]
                , Icon.view "list"
                    [ css "float" "right"
                    , onMouseDown (SelectAction (ExecCmd (Request.loadDocTokens doc.document_id)))
                ]
                ]
            , span
                [ Color.text (Color.color Color.Grey Color.S200)
                , css "padding" "2px"
                , css "font-size" "8px"
                , css "align-self" "right"
                ]
                [ text (toString doc.time_stamp) ]
            ]
        , Card.text
            [ Color.text (Color.white)
            , css "padding" "4px"
            , css "font-size" "10px"
            ]
            [ text doc.snippet ]
        ]
