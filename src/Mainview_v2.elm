module Mainview_v2 exposing (view)

import Model exposing (..)
import Tabsview
import Topicsview
import Savedview
import Documentsview
import Termsview
import Searchview

import Array
import Html exposing (Html, text, h3)
import Material.Options exposing (Property, css, cs, center, div, span, onToggle, onClick, onInput, dispatch)
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
            { header =
                (if model.settings.error == ""
                then
                    [ viewSearch model]
                else
                    [ viewSearch model
                    , span
                        [ css "float" "right"
                        , css "width" "300px"
                        , css "display" "inline"
                        ]
                        [ text model.settings.error]
                    ]
                )
            , drawer = [ viewSwitch model]
            , tabs = ( [], [])
            , main =
                [ viewBody model
                ]
            }
        |> Scheme.topWithScheme Color.Green Color.Orange

viewSearch : Model -> Html Msg
viewSearch model =
    span
        [ cs "search_box"
        , primaryColor
        ]
        [ Textfield.render Mdl [7] model.mdl
            [ Textfield.label "Search"
            , Textfield.floatingLabel
            , Textfield.text_
        --    , Textfield.value <value> to set the value manualy if the label is not floating, wehn it should
        --    , Textfield.expandable "id-of-expandable-1"
        --    , Textfield.expandableIcon "search"
            , onInput Search
            , dispatch Batch
            ]
            [ ]
        ]

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
        (if model.settings.search
        then
            [ Searchview.view model (flexValue (-1))]
        else
            [ div
                [ cs "flex__row"
                , css "flex" "3 2 70%"
                , css "height" "100%"
                ]
--                (List.append
                [ if (not (List.isEmpty model.slots.more))
                    then slotAction (onClick None) "<"
                    else div [] []
                , div
                    [ cs "flex__row"
                    , css "flex" (flexValue ((slotsCount model.slots) * 2))
                    ]
--                    (List.map hiddenSlot (List.reverse (List.range 1 (List.length model.slots.more)))))
                    (Array.toList (Array.indexedMap (slot model) model.slots.main))
                , if ((slotGet model.slots 2) == Empty)
                    then slotAction (onClick (ChoseSlotDialog (slotGetFirstId model.slots Empty))) "add"
                    else slotAction (onClick None) ">"
                , Tabsview.view model (flexValue 6)
                ]
            , Savedview.view model "1 1 30%"
            ]
        )

slotAction : Property c Msg -> String -> Html Msg
slotAction action icon =
    div
        [ generalBackgroundColor
        , center
        , action
        ]
        [ Icon.view icon [ Icon.size48 ]
        ]

slot : Model -> Int -> View -> Html Msg
slot model slotId view =
    case view of
        TopicsView name topics contId ->
            Topicsview.view { model | topics = topics, topicsContainer = contId} (flexValue 2) slotId name
        TermsView name terms ->
            Termsview.view { model | terms = terms} (flexValue 2) slotId name
        DocumentsView name docs ->
            Documentsview.view { model | docs = docs} (flexValue 2) slotId name
        Dialog ->
            div
                [ cs "slot"
                , cs "slot__new"
                , primaryColor
                ]
                [ Button.render Mdl [slotId] model.mdl
                    [ cs "slot__close_button"
                    , Button.fab
                    , Button.minifab
                    , Button.raised
                    , Button.ripple
                    , onClick (DeleteSlot slotId)
                    ]
                    [ Icon.i "close" ]
                , div
                    [ center
                    , cs "slot__content"
                    , cs "flex__column"
                    ]
                    [ slotDialogCard "Topics" (TopicsView "Topics" model.topics model.topicsContainer) slotId
                    , slotDialogCard "Terms" (TermsView "Terms" model.terms) slotId
                    , slotDialogCard "Documents" (DocumentsView "Documents" model.docs) slotId
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
                --    , onClick (UpdateSlot (WordlistView model.terms) slotId)
                --    ]
                --    [ text "Terms"]
                --, Button.render Mdl [6] model.mdl
                --    [ Button.ripple
                --    , css "flex" "flexValue 1"
                --    , css "margin" "70px 0"
                --    , onClick (UpdateSlot (DocumentsView model.docs) slotId)
                --    ]
                --    [ text "Documents"]
                    ]
                ]
        Empty ->
--            let previouseSlot = (slotGet model.slots (slotId - 1))
--            in
--            if ((previouseSlot /= Empty) && (previouseSlot /= Dialog))
--            if (slotId == 0)
--                then
--                    div
--                        [ cs "slot"
--                        , cs "slot__half"
--                        , primaryColor
--                        , center
--                        , onClick (ChoseSlotDialog slotId)
--                        ]
--                        [ Icon.view "add" [Icon.size48]
--                        ]
--                else
                    div [] []
        _ ->
            div [][ text "Error"]

hiddenSlot : Int -> Html Msg
hiddenSlot id =
    div
        [ cs "slot__hidden"
        , center
        , primaryColor
        , onClick None
        ]
        [ text (toString id)]

slotDialogCard : String -> View -> Int -> Html Msg
slotDialogCard title view slotId =
    Card.view
        [ css "height" "20%"
        , css "width" "100%"
        , css "margin" "15px 0"
        , primaryColor
        , onClick (UpdateSlot view slotId)
        ]
        [ Card.title
            [ css "height" "100%"
            , center
            --, Color.text Color.white
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
