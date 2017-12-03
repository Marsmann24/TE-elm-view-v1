module Mainview_v2 exposing (view)

import Model exposing (..)
import Tabsview
import Topicsview
import Savedview
import Articlesview
import Wordlistview

import Html exposing (Html, text, h3)
import Material.Options exposing (css, cs, center, div, span, onToggle, onClick)
import Material.Icon as Icon
import Material.Color as Color
import Material.Scheme as Scheme
import Material.Layout as Layout
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Button as Button
import Material.Card as Card
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
        , Toggles.switch Mdl [1] model.mdl
            [ css "margin" "5px"
            , onToggle ToggleView2
            , Toggles.value model.settings.view2
            ]
            [ text "Mainview 2" ]
        , Toggles.switch Mdl [2] model.mdl
            [ css "margin" "5px"
            , onToggle ToggleShowSaved
            , Toggles.value model.settings.showSaved
            ]
            [ text "show Saved"]
        ]

viewBody : Model -> Html Msg
viewBody model =
    div [ Elevation.e4
        , cs "flex__column"
        , css "height" "100%"
        ]
        [ div
            [ cs "flex__row"
            , css "flex" (flexValue 3)
            , css "height" "100%"
            ]
            [ div
                [ cs "flex__row"
                ]
                (List.map hiddenSlot (List.reverse (List.range 1 (List.length model.slots.more))))
            , slot 1 model.slots.s1 model
            , slot 2 model.slots.s2 model
            , slot 3 model.slots.s3 model
            , Tabsview.view model (flexValue 6)
            ]
        , Savedview.view model (flexValue 1)
        ]

slot : Int -> View -> Model -> Html Msg
slot slotId view model =
    case view of
        TopicsView topics->
            Topicsview.view { model | topics = topics} (flexValue 2) slotId
        WordlistView wordList->
            Wordlistview.view { model | wordList = wordList} (flexValue 2) slotId
        ArticlesView articles->
            Articlesview.view { model | articles = articles} (flexValue 2) slotId
        Dialog ->
            div
                [ cs "slot"
                , cs "flex__column"
                , css "flex" (flexValue 1)
                , Color.background (Color.color Color.Grey Color.S500)
                , center
                ]
                [ slotDialogCard "Topics" (TopicsView model.topics) slotId
                , slotDialogCard "Wordlist" (WordlistView model.wordList) slotId
                , slotDialogCard "Articles" (ArticlesView model.articles) slotId
                --, Button.render Mdl [5] model.mdl
                --    [ Button.ripple
                --    , css "flex" "flexValue 1"
                --    , css "margin" "70px 0"
                --    , onClick (UpdateSlot (TopicsView model.topics) slotId)
                --    ]
                --    [ text "Topics"]
                --, Button.render Mdl [5] model.mdl
                --    [ Button.ripple
                --    , css "flex" "flexValue 1"
                --    , css "margin" "70px 0"
                --    , onClick (UpdateSlot (WordlistView model.wordList) slotId)
                --    ]
                --    [ text "Wordlist"]
                --, Button.render Mdl [6] model.mdl
                --    [ Button.ripple
                --    , css "flex" "flexValue 1"
                --    , css "margin" "70px 0"
                --    , onClick (UpdateSlot (ArticlesView model.articles) slotId)
                --    ]
                --    [ text "Articles"]
                ]
        Empty ->
            let previouseSlot = (slotGet model.slots (slotId - 1))
            in
            if ((previouseSlot /= Empty) && (previouseSlot /= Dialog))
                then
                    div
                        [ cs "slot"
                        , css "flex" (flexValue 1)
                        , Color.background (Color.color Color.Grey Color.S500)
                        , center
                        , onClick (ChoseSlotDialog slotId)
                        ]
                        [ Icon.view "add" []
                        ]
                else
                    div [] []
        _ ->
            div [][ text "Error"]

hiddenSlot : Int -> Html Msg
hiddenSlot id =
    div
        [ cs "slot__hidden"
        , center
        , Color.background (Color.color Color.Grey Color.S500)
        , onClick None
        ]
        [ text (toString id)]

slotDialogCard : String -> View -> Int -> Html Msg
slotDialogCard title view slotId =
    Card.view
        [ css "height" "20%"
        , css "width" "100%"
        , css "margin" "15px 0"
        , Color.background (Color.color Color.Grey Color.S600)
        , onClick (UpdateSlot view slotId)
        ]
        [ Card.title
            [ css "height" "100%"
            , center
            , Color.text Color.white
            ]
            [ text title ]
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
