module Searchview exposing (view)

import Model exposing (..)
import Request

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
            [ Lists.li
                []
                [ Lists.content
                    [ onClick
                        (ExecCmd (Request.loadSearchTopics model.settings.search4))
                    ]
                    [ Lists.icon "bubble_chart" []
                    , text ("Search for Topics with " ++ model.settings.search4)
                    ]
                ]
            , Lists.li
                []
                [ Lists.content
                    [ onClick
                        (ExecCmd (Request.loadSearchTerms model.settings.search4))
                    ]
                    [ Lists.icon "list" []
                    , text ("Search for Terms with " ++ model.settings.search4)
                    ]
                ]
            , Lists.li
                []
                [ Lists.content
                    [ onClick
                        (ExecCmd (Request.loadSearchDocs model.settings.search4 False "RELEVANCE"))
                    ]
                    [ Lists.icon "art_track" []
                    , text ("Search for Documents with " ++ model.settings.search4)
                    ]
                ]
            ]
            --(List.map searchresult2ListItem model.result)
        ]

--searchresult2ListItem : Searchresult -> Html Msg
--searchresult2ListItem result =
    --Lists.li
        --[]
        --[( case result of
        --    Termresult term ->
        --        Lists.content
        --       ,     [ onClick (Found (TermsView "Terms" [term]))]
        --            [ Lists.icon "list" []
        --            , text term.name
        --            ]
        --    Topicresult topic ->
        --        Lists.content
        --            [ onClick (Found (TopicsView "Topics" [topic] 0))]
        --            [ Lists.icon "bubble_chart" []
        --            , text (toString topic.id)
        --            ]
        --    Documentresult doc ->
        --        Lists.content
        --            [ onClick (Found (DocumentsView "Documents" [doc]))]
        --            [ Lists.icon "art_track" []
        --            , text doc.title
        --            ]
        --)]
