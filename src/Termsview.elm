module Termsview exposing (view)

import Model exposing (..)
import Term exposing (..)
import Document
import Topic
import Request

import Html exposing (Html, text)
import Html.Events
import Material.Options exposing (css, cs, div, center, onClick)
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.List as Lists

view : Model -> String -> Int -> Html Msg
view model flex slotId =
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
            [ Icon.view "list" [ css "margin" "5px"]
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
        , Lists.ul
            [ cs "slot__content"
            ]
            (List.map2 (topic2Terms model)
                model.terms
                (List.map (\x -> x + 10 * slotId)
                    (List.range 1 (List.length model.terms))
                )
            )
        ]

topic2Terms : Model -> Term -> Int -> Html Msg
topic2Terms model term id=
    Lists.li
        []
        [ Lists.content
            (if (term.id == model.currentTerm.id)
                then
                    [ cs "mdl-button"
                    , cs "mdl-button--raised"
                    , center
                    , Elevation.e2
                    ]
                else
                    [ cs "mdl-button"
                    , cs "mdl-button--raised"
                    , center
                    ]
            )
            [ text (term.name ++ " (" ++ (toString term.id) ++ ")")
            , Icon.view "bubble_chart"
                [ onClick
                    (ShowTopics
                        (List.filter
                            (Topic.termInTopic term)
                            model.topics
                        ))
                ]
            , Icon.view "art_track"
                [ onClick
                    (ExecCmd (Request.loadBestDocs 0 term.id "relevance"))
                    --(ShowDocuments
                    --    (List.filter
                    --        (Document.termInDocument term)
                    --        model.docs
                    --    )
                    --)
                ]
            ]
        ]
