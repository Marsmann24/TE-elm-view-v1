module Wordlistview exposing (view)

import Model exposing (..)

import Html exposing (Html, text)
import Html.Events
import Material.Options exposing (css, cs, div, attribute, onClick)
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.List as Lists

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
            [ Icon.view "list" [ css "margin" "5px"]
            , Button.render Mdl [slotId] model.mdl
                [ cs "slot__close_button"
                , Button.fab
                , Button.minifab
                , Button.raised
                , Button.ripple
                , onClick (HideWordList slotId)
                ]
                [ Icon.i "close" ]
            ]
        , Lists.ul
            [ cs "slot__content"
            ]
            (List.map2 (topic2WordList model)
                model.wordList
                (List.map (\x -> x+10*slotId)
                    (List.range 1 (List.length model.wordList))
                )
            )
        ]

topic2WordList : Model -> String -> Int -> Html Msg
topic2WordList model word id=
    Lists.li
        []
        [ Lists.content
            (if (word == model.currentWord)
                then
                    [ Elevation.e2]
                else
                    [ attribute <| Html.Events.onClick (ShowArticles word)])
            [ Button.render Mdl [ id ] model.mdl
                [ Button.ripple
                , Button.raised
                ]
                [ text word]
            ]
        ]
