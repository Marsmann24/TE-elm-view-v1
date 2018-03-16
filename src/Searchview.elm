module Searchview exposing (view)

import Model exposing (..)
import Request
import Topic exposing (Topic)
import Term exposing (Term)

import Html exposing (Html, text)
import Material.Options exposing (div, span, cs, css, center, onClick)
import Material.List as Lists
import Material.Elevation as Elevation

view : Model -> String -> Html Msg
view model flex =
    div
        [ cs "search_results"
        --, css "flex" flex
        , primaryColor
        --, center
        ]
        [ Lists.ul
            [ css "overflow" "auto"]
            (List.append
                [ if (String.startsWith "topics:" model.settings.search4)
                  then
                    (li "bubble_chart"
                        ("Search for Topics with " ++ model.settings.search4)
                        (ExecCmd (Request.loadSearchTopics (String.dropLeft 6 model.settings.search4)))
                    )
                  else span [] []
                , if (String.startsWith "terms:" model.settings.search4)
                  then
                      (li "list"
                        ("Search for Terms with " ++ model.settings.search4)
                        (ExecCmd (Request.loadSearchTerms (String.dropLeft 6 model.settings.search4)))
                    )
                  else span [] []
                , if (String.startsWith "documents:" model.settings.search4)
                  then
                      (li "art_track"
                        ("Search for Documents with " ++ model.settings.search4)
                        (ExecCmd (Request.loadSearchDocs (String.dropLeft 6 model.settings.search4 False "RELEVANCE")))
                    )
                  else span [] []
                ]
                (searchresult2ListItems model.settings.searchResult)
            )
        ]

searchresult2ListItems : SearchResult -> List (Html Msg)
searchresult2ListItems result =
    case result of
        TermResult list ->
            let lia : Term -> Html Msg
                lia a =
                    li "list" a.name (Found (TermsView "Terms" [a]))
            in
            List.map lia list
        TopicResult list ->
            let lia : Topic -> Html Msg
                lia a =
                    li "bubble_chart" (toString a.id) (Found (TopicsView "Topics" [a] 0))
            in
            List.map lia list
        DocumentResult a ->
            [ li "art_track" a.title (Found (DocumentsView "Documents" [a]))]

li : String -> String -> Msg -> Html Msg
li icon label msg =
    Lists.li
        [ Elevation.e4
        , css "margin" "20px 0 20px 0"
        ]
        [ Lists.content
            [ onClick msg]
            [ Lists.icon icon []
            , text label
            ]
        ]
