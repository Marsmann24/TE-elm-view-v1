module Termsview exposing (view)

import Model exposing (..)
import Term exposing (..)
import Document
import Topic
import Request

import Html exposing (Html, text)
import Html.Events
import Material.Options exposing (css, cs, div, span, center, onClick, nop)
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.List as Lists

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
            , center
            ]
            [ Icon.view "list" [ css "margin" "5px"]
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
        , Lists.ul
            [ cs "slot__content"
            ]
            (List.indexedMap (topic2Terms model slotId) model.terms)
        ]

topic2Terms : Model -> Int -> Int -> Term -> Html Msg
topic2Terms model slotId id term =
    Lists.li
        [ css "overflow" "visible"
        ]
        [ Lists.content
            [ cs "mdl-button"
            , cs "mdl-button--raised"
            , css "overflow" "visible"
            , center
            , if (term.id == model.currentTerm.id)
                then Elevation.e2
                else nop
            ]
            [ span
                [ css "width" "calc(100% - 48px)"
                , onClick (SelectItem (slotId, id))
                ]
                [ text term.name
                --, text (term.name ++ " (" ++ (toString term.id) ++ ")")
                ]
            , span
                [ onClick
                    (ExecCmd (Request.loadAutocompleteTerms term.name))
                --    (ShowTopics
                --        (List.filter
                --            (Topic.termInTopic term)
                --            model.topics
                --        ))
                ]
                [ Icon.view "bubble_chart" (iconHighlighted model.settings (slotId, id))]
            , span
                [ onClick
                    (ExecCmd (Request.loadBestDocs Topic.defaultTopic (Just term) "RELEVANCE"))
                    --(ShowDocuments
                    --    (List.filter
                    --        (Document.termInDocument term)
                    --        model.docs
                    --    )
                    --)
                ]
                [ Icon.view "art_track" (iconHighlighted model.settings (slotId, id))]
            ]
        ]
