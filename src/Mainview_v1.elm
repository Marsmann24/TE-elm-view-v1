module Mainview_v1 exposing (view)

import Model exposing (..)
import Tabsview
import Topicsview
import Savedview
import Articlesview
import Wordlistview

import Html exposing (Html, text, h3)
import Material.Options exposing (css, div, onToggle)
import Material.Color as Color
import Material.Scheme as Scheme
import Material.Layout as Layout
import Material.Textfield as Textfield
import Material.Toggles as Toggles

view : Model -> Html Msg
view model =
        Layout.render Mdl model.mdl
            [ Layout.fixedHeader]
            { header =[ viewSearch model]
            , drawer = [ viewSwitch model]
            , tabs = ( [], [])
            , main =
                [ if model.settings.view2
                    then
                        viewBody2 model
                    else
                        viewBody model
                ]
            }
        |> Scheme.topWithScheme Color.LightGreen Color.Lime

viewSearch : Model -> Html Msg
viewSearch model =
    Textfield.render Mdl [7] model.mdl
        [ Textfield.label "Expandable"
        , Textfield.floatingLabel
        , Textfield.expandable "id-of-expandable-1"
        , Textfield.expandableIcon "search"
        , css "padding" "20px 50px 10px"
        ]
        []

viewSwitch : Model -> Html Msg
viewSwitch model =
    div
        []
        [ h3 [] [ text "Einstellungen"]
        , Toggles.switch Mdl [0] model.mdl
            [ css "margin" "5px"
            , onToggle ToggleBottom
            , Toggles.value model.settings.bottom
            ]
            [ text "Topics at bottom" ]
        , Toggles.switch Mdl [0] model.mdl
            [ css "margin" "5px"
            , onToggle ToggleView2
            , Toggles.value model.settings.view2
            ]
            [ text "Mainview 2" ]
        , Toggles.switch Mdl [0] model.mdl
            [ css "margin" "5px"
            , onToggle ToggleShowSaved
            , Toggles.value model.settings.showSaved
            ]
            [ text "show Saved"]
        , Toggles.switch Mdl [0] model.mdl
            [ css "margin" "5px"
            , onToggle ToggleShowArticles
            , Toggles.value model.settings.showArticles
            ]
            [ text "show Articles"]
        ]

viewBody : Model -> Html Msg
viewBody model =
    div [ css "display" "flex"
        , css "display" "-ms-flex"
        , css "display" "-webkit-flex"
        , css "flex-direction" "column"
        , css "-ms-flex-direction" "column"
        , css "-webkit-flex-direction" "column"
        , css "height" "100%"
        ]
        [ div
            [ css "display" "flex"
            , css "display" "-ms-flex"
            , css "display" "-webkit-flex"
            , css "flex-direction" "row"
            , css "-ms-flex-direction" "row"
            , css "-webkit-flex-direction" "row"
            , css "flex" (flexValue 3)
            ]
            [ viewOrEmptyFlex (not model.settings.bottom) (Topicsview.view model (flexValue 1) 1)
            , viewOrEmptyFlex model.settings.showWordlist (Wordlistview.view model (flexValue 1) 2)
            , viewOrEmptyFlex model.settings.showArticles (Articlesview.view model (flexValue 1) 3)
            , Tabsview.view model (flexValue (5 - (countOther model.settings)))
            ]
        , viewOrEmptyFlex model.settings.bottom (bottomView model (flexValue 1))
        , viewOrEmptyFlex ((not model.settings.bottom) && model.settings.showSaved) (Savedview.view model (flexValue 1))
        ]

viewBody2 : Model -> Html Msg
viewBody2 model =
    div [ css "display" "flex"
        , css "display" "-ms-flex"
        , css "display" "-webkit-flex"
        , css "flex-direction" "row"
        , css "-ms-flex-direction" "row"
        , css "-webkit-flex-direction" "row"
        , css "height" "100%"
        ]
        [ div
            [ css "display" "flex"
            , css "display" "-ms-flex"
            , css "display" "-webkit-flex"
            , css "flex-direction" "column"
            , css "-ms-flex-direction" "column"
            , css "-webkit-flex-direction" "column"
            , css "flex" (flexValue (countOther model.settings))
            ]
            [ div
                [ css "display" "flex"
                , css "display" "-ms-flex"
                , css "display" "-webkit-flex"
                , css "flex-direction" "row"
                , css "-ms-flex-direction" "row"
                , css "-webkit-flex-direction" "row"
                , css "flex" (flexValue 3)
                ]
                [ viewOrEmptyFlex (not model.settings.bottom) (Topicsview.view model (flexValue 1) 1)
                , viewOrEmptyFlex model.settings.showWordlist (Wordlistview.view model (flexValue 1) 2)
                , viewOrEmptyFlex model.settings.showArticles (Articlesview.view model (flexValue 1) 3)
                ]
            , viewOrEmptyFlex (model.settings.showSaved) (Savedview.view model (flexValue 1))
            ]
        , Tabsview.view model (flexValue (5 - (countOther model.settings)))
        ]

bottomView : Model -> String -> Html Msg
bottomView model flex =
    div
        [ css "display" "flex"
        , css "display" "-ms-flex"
        , css "display" "-webkit-flex"
        , css "flex-direction" "row"
        , css "flex" flex
        , css "margin" "3px 0px"
        ]
        [ viewOrEmptyFlex model.settings.showSaved (Savedview.view model (flexValue (countOther model.settings)))
        , Topicsview.view model (flexValue (5-(countOther model.settings))) 0
        ]

countOther : Settings -> Int
countOther settings =
    (bool2Int settings.showArticles) + (bool2Int (not settings.bottom)) + (bool2Int settings.showWordlist)

bool2Int : Bool -> Int
bool2Int bool =
    if bool
        then 1
        else 0

viewOrEmptyFlex : Bool -> Html Msg -> Html Msg
viewOrEmptyFlex condition view =
    if condition
        then view
        else div [ css "flex" (flexValue 0)] []

flexValue : Int -> String
flexValue flex =
    case flex of
        4 ->
            "4 3 80%"
        3 ->
            "3 2 60%"
        2 ->
            "2 2 40%"
        1 ->
            "1 1 20%"
        0 ->
            "0 0 0%"
        _ ->
            "1 1 100%"
