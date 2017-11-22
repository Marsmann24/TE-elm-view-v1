module Articlesview exposing (view)

import Model exposing (..)

import Html exposing (Html, text)
import Material.Options exposing (css, div, span, onClick, onMouseEnter, onMouseLeave)
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.Color as Color
import Material.Card as Card

view : Model -> String -> Int -> Html Msg
view model flex slotId =
    div
        [ css "flex" flex
        , css "margin" "3px 0px"
        , Elevation.e4
        ]
        (List.append
            [ div
                [ css "height" "45px"
                ]
                [ Button.render Mdl [0] model.mdl
                    [ Button.fab
                    , Button.minifab
                    , Button.raised
                    , Button.ripple
                    , onClick (HideWordList slotId)
                    , css "margin" "2px 4px"
                    , css "float" "right"
                    ]
                    [ Icon.i "close" ]
                ]
            ]
            (List.map2 (article2CardView model) model.articles (List.range 1 (List.length model.articles))))

article2CardView : Model -> Article -> Int -> Html Msg
article2CardView model article cardID =
    Card.view
        [ css "height" "100px"
        , css "width" "94%"
        , css "margin" "4% 3%"
        , Color.background (Color.color Color.Brown Color.S500)
        , if (cardID == model.currentArticle.cardID)
            then Elevation.e0
          else if (cardID == model.raised)
            then Elevation.e16
          else Elevation.e8
        , Elevation.transition 250
        , onMouseEnter (Raise cardID)
        , onMouseLeave (Raise -1)
        , onClick (ChangeCurrentArticle cardID article)
        ]
        [ Card.title
            [ css "padding" "4px"
            ]
            [ Card.head
                [ Color.text Color.white ]
                [ text article.title ]
            , span
                [ Color.text (Color.color Color.Grey Color.S200)
                , css "padding" "2px"
                , css "font-size" "8px"
                , css "align-self" "right"
                ]
                [ text article.date ]
            ]
        , Card.text
            [ Color.text (Color.white)
            , css "padding" "4px"
            ]
            [ text article.text ]
        ]
