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
    --if model.settings.view2
    --    then
            Mainview_v2.view model
    --    else
    --        Mainview_v1.view model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SelectTab tabNumber ->
            ({ model | currentTab = tabNumber}, Cmd.none)
        Raise cardNumber ->
            ({ model | raised = cardNumber}, Cmd.none)
        --ChangeCurrentDocument cardNumber newDocument->
        --    let newCurrentDocument = { cardID = cardNumber, document = newDocument}
        --    in
        --    ({ model | currentDocument = newCurrentDocument}, Cmd.none)
        RemoveTopic topicID ->
            let newCurrentTopis = List.filter (\x -> x.id /= topicID) model.currentTopics
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
        ShowTerms terms ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | showTerms = True}
        --        , termList = terms
                , slots = slotFromTo oldSlots Empty (TermsView terms)
                }
            , Cmd.none)
        HideTerms slotId ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | showTerms = False}
                , slots =  slotRemove oldSlots slotId
                }
            , Cmd.none)
        ShowDocuments documents ->
            let oldSettings = model.settings
                oldSlots = model.slots
                docs = model.docs
                contains term document = List.member term document.terms
            in
            ({ model
                | settings = { oldSettings | showDocuments = True}
                , slots = slotFromTo oldSlots Empty (DocumentsView docs)
                }
            , Cmd.none)
        HideDocuments slotId ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | showDocuments = False}
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
        NewTopics result ->
            case result of
                Ok topicsResult ->
                    ({ model | topics = topicsResult.topics}, Cmd.none)
                Err _ ->
                    (model, Cmd.none)
        NewDocument result ->
            case result of
                Ok document ->
                    let oldTabs = model.tabs
                    in
                    ({ model | tabs = (List.append oldTabs [DocumentTab document.title document])}, Cmd.none)
                Err _ ->
                    (model, Cmd.none)
        NewDocs result ->
            case result of
                Ok newDocs ->
                    ({ model | docs = newDocs}, Cmd.none)
                Err _ ->
                    (model, Cmd.none)
        NewTerms result ->
            case result of
                Ok newTerms ->
                    ({ model |terms = newTerms}, Cmd.none)
                Err _ ->
                    (model, Cmd.none)
        NewFrames result ->
            (model, Cmd.none)
        ExecCmd cmd ->
            (model, cmd)
        Mdl msgmdl ->
            (Material.update Mdl msgmdl model)
        _ ->
            ( model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
