module Searchview exposing (view)

import Model exposing (..)

import Html exposing (Html, text)
import Material.Options exposing (div, css, center, onClick)
import Material.List as Lists

view : Model -> String -> Html Msg
view model flex =
    div
        [ css "flex" flex
        , center
        ]
        [ Lists.ul
            []
            (List.map searchresult2ListItem model.result)
        ]

searchresult2ListItem : Searchresult -> Html Msg
searchresult2ListItem result =
    Lists.li
        []
        [( case result of
            Termresult term ->
                Lists.content
                    [ onClick (Found (TermsView [term]))]
                    [ Lists.icon "list" []
                    , text term.name
                    ]
            Topicresult topic ->
                Lists.content
                    [ onClick (Found (TopicsView [topic] 0))]
                    [ Lists.icon "bubble_chart" []
                    , text (toString topic.id)
                    ]
            Documentresult doc ->
                Lists.content
                    [ onClick (Found (DocumentsView [doc]))]
                    [ Lists.icon "art_track" []
                    , text doc.title
                    ]
        )]
