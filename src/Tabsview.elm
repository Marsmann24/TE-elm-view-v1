module Tabsview exposing (view)

import Model exposing (..)
import Document exposing (Document)

import Html exposing (Html, text, h1, br)
import Material.Options exposing (css, cs, div, span)
import Material.Tabs as Tabs

view : Model -> String -> Html Msg
view model flex =
    div
        [ cs "slot"
        , css "flex" flex
        , css "padding" "0px 10px"
        , primaryColor
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
                      document2DocumentView model.currentDocument.document
                  DocumentTab _ document ->
                      document2DocumentView document
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
            DocumentTab tabID _ ->
                text tabID
            ErrorTab tabID _ ->
                text tabID
        ]

document2DocumentView : Document -> Html Msg
document2DocumentView document =
    div [ css "margin" "0px 6px"]
        [ span
            [ css "float" "right"
            , css "margin" "4px"
            , css "color" "grey"
            ]
            [ text (toString document.time_stamp)
            , br [][]
            ]
        , h1 [] [ text document.title]
        , span [] [ text document.fulltext]
        ]
