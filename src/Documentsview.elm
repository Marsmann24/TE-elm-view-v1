module Documentsview exposing (view)

import Model exposing (..)
import Document exposing (..)
import Topic exposing (iconTopic)
import Term exposing (iconTerm)
import Request

import Html exposing (Html, text)
import Material.Options exposing (Property, css, cs, div, span, center, onClick, onMouseEnter, onMouseLeave, onMouseDown, onMouseUp)
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.Color as Color
import Material.Card as Card

view : Model -> Property c Msg -> Int -> String -> Parent -> Html Msg
view model flex slotId slotName parent =
    div
        [ cs "slot"
        , if (getActive parent model.settings)
            then cs "active"
            else cs "unactive"
        , if (slotId == model.settings.slotToDelete)
            then cs "slot__remove"
            else flex
        , Elevation.e0
        -- , primaryColor
        , onMouseEnter (SetParent parent)
        , onMouseLeave (SetParent Noparent)
        ]
        [ div
            [ css "height" "45px"
            , css "max-width" "400px"
            , center
            ]
            [ iconDoc [ css "margin" "5px"]
            , span
                [ css "width" "calc(100% - 64px)"
                , css "text-align" "left"
                ]
                [ text slotName]
            , Button.render Mdl [slotId] model.mdl
                [ cs "slot__close_button"
                , Button.fab
                , Button.minifab
                , Button.raised
                , Button.ripple
                , onClick (DeleteSlot slotId)
                ]
                [ Icon.i "close" ]
            ]
        , div
            [ cs "slot__content"
            , css "max-width" "400px"
            ]
            (List.map2 (doc2CardView parent model slotId) model.docs (List.range 1 (List.length model.docs)))
        ]

doc2CardView : Parent -> Model -> Int -> Doc -> Int -> Html Msg
doc2CardView parent model slotId doc cardID =
    Card.view
        [ css "height" "100px"
        , css "width" "94%"
        , css "margin" "4% 8% 4% 3%"
        , if (getActive parent model.settings)
            then Color.background Color.primary
            else if  (isParent doc.id model.settings)
                then cs "active__doc"
                else cs "unactive__doc"
        -- , Color.background Color.primary
        , if (cardID == model.currentDocument.cardID)
            then Elevation.e0
          else if (cardID == model.raised)
            then Elevation.e16
          else Elevation.e8
        , Elevation.transition 250
        , onMouseEnter (Raise cardID)
        , onMouseLeave (Raise -1)
        --, onClick (ChangeCurrentDoc cardID doc)
        , onMouseUp (ExecuteActionIfNone (ExecCmd (Request.loadDoc doc)))
        ]
        [ Card.title
            [ css "padding" "4px"
            ]
            [ Card.head
                [ Color.text Color.white
                , css "font-size" "14px"
                , css "width" "100%"
                ]
                [ span
                    [ css "width" "calc(100% - 48px)"]
                    [ text doc.title]
                --, span
                --    [ onMouseDown
                --        (SelectAction
                --            None
--                            (ShowTopics
--                                (List.filterMap
--                                    (Topic.topicId2Topic model.topics)
--                                    doc.top_topic
--                                )
--                            )
                --        )
                --    ]
                --    [ iconTopic []]
                -- , span
                --     [ onMouseDown (SelectAction (ExecCmd (Request.loadDocTokens doc slotId)))
                --     ]
                --     [ iconTerm []]
                ]
            , span
                [ Color.text (Color.color Color.Grey Color.S200)
                , css "padding" "2px"
                , css "font-size" "8px"
                , css "align-self" "right"
                ]
                [ text (("id=" ++ (toString doc.id)) ++ (" | date=" ++ (toString doc.time_stamp))) ]
            ]
        , Card.text
            [ Color.text (Color.white)
            , css "padding" "4px"
            , css "font-size" "10px"
            ]
            [ text doc.snippet ]
        ]
