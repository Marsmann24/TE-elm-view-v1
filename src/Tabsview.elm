module Tabsview exposing (view)

import Model exposing (..)

import Html exposing (Html, text, h1, br)
import Material.Options exposing (css, div, span)
import Material.Tabs as Tabs

view : Model -> String -> Html Msg
view model flex =
    div
        [ css "flex" flex
        ]
        [ Tabs.render Mdl [0] model.mdl
            [ Tabs.ripple
            , Tabs.onSelectTab SelectTab
            , Tabs.activeTab model.currentTab
            ]
            (List.map tab2TabView model.tabs)
            [ let maybetab = (List.head (List.drop model.currentTab model.tabs))
              in let tab =
                  case maybetab of
                        Just a ->
                            a
                        _ ->
                            ErrorTab "Error" "No Tabs available."
              in
              case tab of
                  PreviewTab ->
                      article2ArticleView model.currentArticle.article
                  ArticleTab _ article ->
                      article2ArticleView article
                  ErrorTab _ errormessage ->
                      text errormessage
            ]
        ]

tab2TabView : Tab -> Tabs.Label Msg
tab2TabView tab =
    Tabs.label
        []
        [ span
            [ css "width" "4px"]
            []
        , case tab of
            PreviewTab ->
                text "Vorschau"
            ArticleTab tabID _ ->
                text tabID
            ErrorTab tabID _ ->
                text tabID
        ]

article2ArticleView : Article -> Html Msg
article2ArticleView article =
    div [ css "margin" "0px 6px"]
        [ span
            [ css "float" "right"
            , css "margin" "4px"
            , css "color" "grey"
            ]
            [ text article.date
            , br [][]
            ]
        , h1 [] [ text article.title]
        , span [] [ text article.text]
        ]
