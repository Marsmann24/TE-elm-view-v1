module Designdummy exposing (..)

import Model exposing (..)
import Init exposing (init)
import Tabsview
import Topicsview
import Articlesview
import Wordlistview

import Html exposing (Html)
import Material
import Material.Options exposing (css, div)
import Material.Color as Color
import Material.Scheme as Scheme
import Material.Layout as Layout
import Material.Textfield as Textfield

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SelectTab tabNumber ->
            ({ model | currentTab = tabNumber}, Cmd.none)
        Raise cardNumber ->
            ({ model | raised = cardNumber}, Cmd.none)
        ChangeCurrentArticle cardNumber newArticle->
            let newCurrentArticle = { cardID = cardNumber, article = newArticle}
            in
            ({ model | currentArticle = newCurrentArticle}, Cmd.none)
        RemoveTopic topicID ->
            let newCurrentTopis = List.filter (\x -> x.topicID /= topicID) model.currentTopics
            in
            ({ model | currentTopics = newCurrentTopis}, Cmd.none)
        ShowWordList words ->
            ({ model | row = True, wordList = words}, Cmd.none)
        HideWordList ->
            ({ model | row = False}, Cmd.none)
        Mdl msgmdl ->
            (Material.update Mdl msgmdl model)
        _ ->
            ( model, Cmd.none)

view : Model -> Html Msg
view model =
        Layout.render Mdl model.mdl
            [ Layout.fixedHeader]
            { header = [ viewHeader model ]
            , drawer = []
            , tabs = ( [], [])
            , main = [ viewBody model]
            }
        |> Scheme.topWithScheme Color.LightGreen Color.Lime

viewHeader : Model -> Html Msg
viewHeader model =
    Textfield.render Mdl [7] model.mdl
        [ Textfield.label "Expandable"
        , Textfield.floatingLabel
        , Textfield.expandable "id-of-expandable-1"
        , Textfield.expandableIcon "search"
        , css "padding" "20px 50px 10px"
        ]
        []

viewBody : Model -> Html Msg
viewBody model =
    div [ css "display" "flex"
        , css "display" "-ms-flex"
        , css "display" "-webkit-flex"
        , css "flex-direction" "column"
        , css "height" "100%"
        ]
        [ div
            [ css "display" "flex"
            , css "display" "-ms-flex"
            , css "display" "-webkit-flex"
            , css "flex-direction" "row"
            , css "-ms-flex-direction" "row"
            , css "-webkit-flex-direction" "row"
            , css "flex" "3 2 75%"
            ]
            (List.append
                [ Articlesview.view model (flexValue 1)]
                (if model.row
                    then
                        [ Wordlistview.view model (flexValue 1)
                        , Tabsview.view model (flexValue 3)
                        ]
                    else
                        [ Tabsview.view model (flexValue 4)]))
        , Topicsview.view model
        ]

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
        _ ->
            "0 0 0%"
