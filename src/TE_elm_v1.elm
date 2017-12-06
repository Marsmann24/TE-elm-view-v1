module TE_elm_v1 exposing (..)

import Model exposing (..)
import Init exposing (init)
--import Mainview_v1 exposing (view)
import Mainview_v1
import Mainview_v2

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

view model =
    if model.settings.view2
        then
            Mainview_v2.view model
        else
            Mainview_v1.view model

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
        Search term ->
                let oldSettings = model.settings
                in
                ({ model
                    | settings = { oldSettings | search = True}
                    --, result = List.filter (\x -> (x == term)) []
                    }
                , Cmd.none)
        Found view ->
                let oldSettings = model.settings
                    oldSlots = model.slots
                in
                ({ model
                    | settings = { oldSettings | search = False}
                    , slots = slotFromTo oldSlots Empty view
                    }
                , Cmd.none)
        ShowTopics topics ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | showTopics = True}
                , slots = slotFromTo oldSlots Empty (TopicsView topics)
                }
            , Cmd.none)
        HideTopics slotId ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | showTopics = False}
                , slots =  slotRemove oldSlots slotId
                }
            , Cmd.none)
        ShowWordList words ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | showWordlist = True}
                , wordList = words
                , slots = slotFromTo oldSlots Empty (WordlistView words)
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
        ShowArticles word ->
            let oldSettings = model.settings
                oldSlots = model.slots
                articles = model.articles
                contains word article = List.member word article.words
            in
            ({ model
                | settings = { oldSettings | showArticles = True}
                , slots = slotFromTo oldSlots Empty (ArticlesView (List.filter (contains word) articles))
                }
            , Cmd.none)
        HideArticles slotId ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | showArticles = False}
                , slots =  slotRemove oldSlots slotId
                }
            , Cmd.none)
        ChoseSlotDialog slotId ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | showSlotDialoge = True}
                , slots =  slotChangeTo oldSlots slotId Dialog
                }
            , Cmd.none)
        UpdateSlot view slotId ->
            let oldSlots = model.slots
            in
            ({ model | slots =  slotChangeTo oldSlots slotId view}, Cmd.none)
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
        Mdl msgmdl ->
            (Material.update Mdl msgmdl model)
        _ ->
            ( model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
