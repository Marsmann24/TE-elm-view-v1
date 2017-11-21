module TE_elm_v1 exposing (..)

import Model exposing (..)
import Init exposing (init)
--import Mainview_v1 exposing (view)
import Mainview_v2 exposing (view)

import Html
import Material

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

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
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | showWordlist = True}
                , wordList = words
                , slots = slotFromTo oldSlots Empty WordlistView
                }
            , Cmd.none)
        HideWordList slotId ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | showWordlist = False}
                , slots =  slotRemove oldSlots slotId
                }
            , Cmd.none)
        ToggleBottom ->
            let oldSettings = model.settings
            in
            ({ model | settings = { oldSettings | bottom = not (model.settings.bottom)}}, Cmd.none)
        ToggleView2 ->
            let oldSettings = model.settings
            in
            ({ model | settings = { oldSettings | view2 = not (model.settings.view2)}}, Cmd.none)
        ToggleShowSaved ->
            let oldSettings = model.settings
            in
            ({ model | settings = { oldSettings | showSaved = not (model.settings.showSaved)}}, Cmd.none)
        ToggleShowArticles ->
            let oldSettings = model.settings
            in
            ({ model | settings = { oldSettings | showArticles = not (model.settings.showArticles)}}, Cmd.none)
        ChoseSlotDialoge ->
            let oldSettings = model.settings
            in
            ({ model | settings = { oldSettings | showSlotDialoge = True}}, Cmd.none)
        Mdl msgmdl ->
            (Material.update Mdl msgmdl model)
        _ ->
            ( model, Cmd.none)

slotFromTo : Slots -> View -> View -> Slots
slotFromTo oldSlots from to =
    if (oldSlots.s1 == from)
        then
            slotChangeTo oldSlots 1 to
        else if (oldSlots.s2 == from)
            then
                slotChangeTo oldSlots 2 to
            else
                slotChangeTo oldSlots 3 to

slotChangeTo : Slots -> Int -> View -> Slots
slotChangeTo oldSlots id value =
    case id of
        1 ->
            { oldSlots | s1 = value}
        2 ->
            { oldSlots | s2 = value}
        3 ->
            { oldSlots | s3 = value}
        _ ->
            oldSlots

slotRemove : Slots -> Int -> Slots
slotRemove oldSlots id =
    slotChangeTo oldSlots id Empty

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
