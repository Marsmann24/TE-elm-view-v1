module Tabsview exposing (view)

import Model exposing (..)
import Document exposing (Document)

import Html exposing (Html, text, h1, br)
import Material.Options exposing (Property, css, cs, div, span, onClick)
import Material.Tabs as Tabs
import Material.Button as Button
import Material.Icon as Icon

view : Model -> Property c Msg -> Html Msg
view model flex =
    div
        [ cs "slot"
        , flex
        , css "padding" "0px 10px"
        -- , css "margin" "3px 0"
        , css "box-shadow" "0 0 10px rgba(0, 0, 0, 0.80)"
        , primaryColor
        ]
        [ Tabs.render Mdl [0] model.mdl
            [ Tabs.ripple
            , Tabs.onSelectTab SelectTab
            , Tabs.activeTab model.currentTab
            , css "height" "100%"
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
    div [ css "margin" "0px 6px"
        , css "height" "calc(100% - 97px)"
        ]
        [ Button.render Mdl [99] model.mdl
            [ cs "slot__close_button"
            , Button.icon
            , onClick CloseTab
            ]
            [ Icon.i "close" ]
        , span
            [ css "float" "right"
            , css "margin" "4px"
            , css "color" "grey"
            ]
            [ text (("id=" ++ (toString document.id)) ++ (" | date=" ++ (toString document.time_stamp)))
            , br [][]
            ]
        , h1 [] [ text document.title]
        , div
            [ css "overflow-y" "auto"
            , css "height" "inherit"
            ]
            [ text document.fulltext]
        ]
