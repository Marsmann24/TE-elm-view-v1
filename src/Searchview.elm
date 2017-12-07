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
            Wordresult word ->
                Lists.content
                    [ onClick (Found (WordlistView [word]))]
                    [ Lists.icon "list" []
                    , text word
                    ]
            Topicresult topic ->
                Lists.content
                    [ onClick (Found (TopicsView [topic]))]
                    [ Lists.icon "bubble_chart" []
                    , text topic.topicName
                    ]
            Articleresult article ->
                Lists.content
                    [ onClick (Found (ArticlesView [article]))]
                    [ Lists.icon "art_track" []
                    , text article.title
                    ]
        )]
