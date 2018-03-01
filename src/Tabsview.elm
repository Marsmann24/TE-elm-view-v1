module Tabsview exposing (view)

import Model exposing (..)
import Document exposing (Document)

import Html exposing (Html, text, h1, br)
import Material.Options exposing (css, cs, div, span, onClick)
import Material.Tabs as Tabs
import Material.Button as Button
import Material.Icon as Icon

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
            (List.map tab2TabView  model.tabs)
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
                      document2DocumentView model model.currentDocument.document
                  DocumentTab _ document ->
                      document2DocumentView model document
                  ErrorTab _ errormessage ->
                      text errormessage
            ]
        ]

tab2TabView : Tab -> Tabs.Label Msg
tab2TabView tab =
    Tabs.label
        []
        (case tab of
            PreviewTab ->
                [ span
                    [ css "width" "4px"]
                    []
                , text "Vorschau"
                ]
            DocumentTab tabID _ ->
                [ span
                    [ css "width" "4px"]
                    []
                , text tabID
                ]
            ErrorTab tabID _ ->
                [ span
                    [ css "width" "4px"]
                    []
                , text tabID
                ]
        )


document2DocumentView : Model -> Document -> Html Msg
document2DocumentView model document =
    div [ css "margin" "0px 6px"]
        [ span
            [ css "float" "right"
            , css "margin" "4px"
            , css "color" "grey"
            ]
            [ text (toString document.time_stamp)
            , br [][]
            ]
        , Button.render Mdl [99] model.mdl
            [ cs "slot__close_button"
            , Button.icon
            , onClick CloseTab
            ]
            [ Icon.i "close" ]
        , h1 [] [ text document.title]
        , span [] [ text document.fulltext]
        ]
