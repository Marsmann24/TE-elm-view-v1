module Mainview_v2 exposing (view)

import Model exposing (..)
import Tabsview
import Topicsview
import Savedview
import Articlesview
import Wordlistview

import Html exposing (Html, text, h3)
import Material.Options exposing (css, center, div, onToggle, onClick)
import Material.Icon as Icon
import Material.Color as Color
import Material.Scheme as Scheme
import Material.Layout as Layout
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Elevation as Elevation

view : Model -> Html Msg
view model =
        Layout.render Mdl model.mdl
            [ Layout.fixedHeader]
            { header =[ viewSearch model]
            , drawer = [ viewSwitch model]
            , tabs = ( [], [])
            , main =
                [ viewBody model
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
            [ slot 1 model.slots.s1 model
            , slot 2 model.slots.s2 model
            , slot 3 model.slots.s3 model
            , Tabsview.view model (flexValue 6)
            ]
        , Savedview.view model (flexValue 1)
        ]

slot : Int -> View -> Model -> Html Msg
slot slotId view model =
    case view of
        TopicsView ->
            Topicsview.view model (flexValue 2) slotId
        WordlistView ->
            Wordlistview.view model (flexValue 2) slotId
        ArticlesView ->
            Articlesview.view model (flexValue 2) slotId
        _ ->
            div
                [ css "flex" (flexValue 1)
                , Elevation.e2
                , center
                , onClick ChoseSlotDialoge
                ]
                [ Icon.view "add" []
                ]

viewOrEmptyFlex : Bool -> Html Msg -> Html Msg
viewOrEmptyFlex condition view =
    if condition
        then view
        else div [ css "flex" (flexValue 0)] []

flexValue : Int -> String
flexValue flex =
    case flex of
        6 ->
            "6 5 50%"
        5 ->
            "5 4 47%"
        4 ->
            "4 3 32%"
        3 ->
            "3 2 25%"
        2 ->
            "2 2 12%"
        1 ->
            "1 1 7%"
        0 ->
            "0 0 0%"
        _ ->
            "1 1 100%"
