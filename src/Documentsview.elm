module Documentsview exposing (view)

import Model exposing (..)
import Document exposing (..)
import Topic

import Html exposing (Html, text)
import Material.Options exposing (css, cs, div, span, onClick, onMouseEnter, onMouseLeave)
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.Color as Color
import Material.Card as Card

view : Model -> String -> Int -> Html Msg
view model flex slotId =
    div
        [ cs "slot"
        , css "flex" flex
        , Elevation.e0
        , Color.background (Color.color Color.Grey Color.S500)
        ]
        [ div
            [ css "height" "45px"
            ]
            [ Icon.view "art_track" [ css "margin" "5px"]
            , Button.render Mdl [slotId] model.mdl
                [ cs "slot__close_button"
                , Button.fab
                , Button.minifab
                , Button.raised
                , Button.ripple
                , onClick (HideTerms slotId)
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
        , onClick (ChangeCurrentDoc cardID doc)
        ]
        [ Card.title
            [ css "padding" "4px"
            ]
            [ Card.head
                [ Color.text Color.white ]
                [ text doc.title
                , Icon.view "bubble_chart"
                    [ onClick
                        (ShowTopics
                            (List.filterMap
                                (Topic.topicId2Topic model.topics)
                                doc.top_topic
                            )
                        )
                    ]
                , Icon.view "list"
                    [ onClick
                        (Request GetDoc ("&DocId=" ++ (toString doc.document_id)))
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
            ]
            [ text doc.snipet ]
        ]
