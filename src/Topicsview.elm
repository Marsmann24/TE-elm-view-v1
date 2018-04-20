module Topicsview exposing (view)

import Model exposing (..)
import Topic exposing (..)
import Term exposing (iconTerm)
import Document exposing (iconDoc)
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
            , css "max-width" "400px"
            , center
            ]
            [ iconTopic [ css "margin" "5px"]
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
                , onClick (DeleteSlot slotId)
                ]
                [ Icon.i "close" ]
            ]
        , div
            [ cs "slot__content"
            , css "max-width" "400px"
            ]
            (List.indexedMap (topic2Chip model.settings slotId) model.topics)
        ]

topic2Chip : Settings -> Int -> Int -> Topic -> Html Msg
topic2Chip settings slotId id topic =
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
                [ css "width" "calc(100% - 58px)"
                , css "overflow" "hidden"
                , css "margin-right" "10px"
                , onClick (SelectItem (slotId, id))
                ]
                [ span [ css "margin-right" "10px"] [ text (("Topic " ++ (toString topic.id)) ++ ": ")]
                , text
                    (String.concat
                        (List.intersperse
                            ", "
                            (List.take 2
                                (List.map .name topic.top_terms)
                            )
                        )
                    )
                , text " ... "
                ]
            , span
                [ onClick
                    (ExecCmd (Request.loadTerms topic 0 slotId))
                    --(ShowTermList topic.top_terms)
                , center
                ]
                [ iconTerm (iconHighlighted settings (slotId, id))]
            , span
                [ onClick
                    (ExecCmd (Request.loadBestDocs topic Nothing "RELEVANCE" slotId))
                    --ShowDocuments
                    --    (List.filter
                    --        (Document.topicInDoc topic)
                    --        model.docs
                    --    ))]
                , center
                ]
                [ iconDoc (iconHighlighted settings (slotId, id))]
            ]
        ]
