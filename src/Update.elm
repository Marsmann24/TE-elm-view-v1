module Update exposing (update)

import Model exposing (..)
import Topic exposing (Topic)
import Document

import Material
import Delay
import Time
import ContainerCache exposing (Page(..))
import Platform.Cmd
import Maybe exposing (withDefault)
import Array

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
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
        SelectTab tabId ->
            ({ model | currentTab = tabId}, Cmd.none)
        CloseTab ->
            let newCurrentTab =
                    model.currentTab - 1
                newTabs =
                    List.append (List.take model.currentTab model.tabs) (List.drop (model.currentTab + 1) model.tabs)
            in
            ({ model
                | currentTab = newCurrentTab
                , tabs = newTabs
            }, Cmd.none)
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
        DeleteSlot slotId msg->
            let oldSettings = model.settings
            in
            ({ model
                | settings = { oldSettings | slotToDelete = slotId}
                }
            , Delay.after 200 Time.millisecond msg)
        RemoveSlotFromOther id ->
            let newSlots = slotRemoveMore model.slots id
            in
            ({ model
                | slots = newSlots
            }, Cmd.none)
        SlotToLastFromOther id ->
            let newSlots = slotMove2EndFromMore model.slots id
            in
            ({ model
                | slots = newSlots
            }, Cmd.none)
        ShowTopics topics ->
            let oldSettings = model.settings
                oldSlots = model.slots
                topicsContainer = model.topicsContainer
            in
            ({ model
                | settings = { oldSettings | showTopics = True}
                , slots = slotFromTo oldSlots Empty (TopicsView "Topics" topics topicsContainer)
                }
            , Cmd.none)
        HideTopics slotId ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | showTopics = False, slotToDelete =-1}
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
                , slots = slotFromTo oldSlots Empty (TermsView "Terms" terms)
                }
            , Cmd.none)
        HideTerms slotId ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | showTerms = False, slotToDelete =-1}
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
                , slots = slotFromTo oldSlots Empty (DocumentsView "Documents" docs)
                }
            , Cmd.none)
        HideDocuments slotId ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | showDocuments = False, slotToDelete =-1}
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
        HideSlot slotId ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | slotToDelete =-1}
                , slots =  slotRemove oldSlots slotId
                }
            , Cmd.none)
        UpdateSlot view slotId ->
            let oldSlots = model.slots
            in
            ({ model | slots = slotChangeTo oldSlots slotId view}, Cmd.none)
        NewTopics result ->
            let oldSettings = model.settings
                oldSlots = model.slots
                topicsContainer = model.topicsContainer
            in
            case result of
                Ok newTopics ->
                    ({ model
                        | settings = { oldSettings | showTopics = True}
                        , slots = slotFromTo oldSlots Empty (TopicsView "Topics" newTopics topicsContainer)
                        , topics = newTopics
                        }
                    , Cmd.none)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, Cmd.none)
        NewTerms result ->
            let oldSettings = model.settings
                oldSlots = model.slots

            in
            case result of
                Ok newTerms ->
                    ({ model
                        | settings = { oldSettings | showTerms = True}
                        , slots = slotFromTo oldSlots Empty (TermsView "Terms" newTerms)
                        , terms = newTerms}
                    , Cmd.none)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, Cmd.none)
        NewDocs result ->
            let oldSettings = model.settings
                oldSlots = model.slots
                contains term document = List.member term document.terms
            in
            case result of
                Ok newDocs ->
                    ({ model
                        | settings = { oldSettings | showDocuments = True}
                        , slots = slotFromTo oldSlots Empty (DocumentsView "Documents" newDocs)
                        , docs = newDocs
                        }
                    , Cmd.none)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, Cmd.none)
        NewDocTokens result ->
            let oldSettings = model.settings
                oldSlots = model.slots
                allTerms = model.terms
            in
            case result of
                Ok document ->
                    ({ model
                        | settings = { oldSettings | showTerms = True}
                        , slots = slotFromTo oldSlots Empty (TermsView "Terms" (Document.documentTerms document allTerms))}
                    , Cmd.none)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, Cmd.none)
        NewDocument result ->
            let oldSettings = model.settings
                oldTabs = model.tabs
                tabNumber = List.length oldTabs
            in
            case result of
                Ok document ->
                    ({ model
                        | tabs = (List.append oldTabs [DocumentTab document.title document])
                        , currentTab = tabNumber}
                    , Cmd.none)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, Cmd.none)
        NewFrames result ->
            (model, Cmd.none)
        ExecCmd cmd ->
            (model, cmd)
        ContainerCacheTopicMsg slotId submsg -> -- Chris Einbindung
            let
                (newdata, cmd ) =
                    ContainerCache.update submsg model.containerTopicModel
                currentPage : ContainerCache.Container (List Topic) -> ContainerCache.Page (List Topic)
                currentPage cont =
                    withDefault (HandleError "") (Array.get cont.meta.currPage cont.data)
                topicList : ContainerCache.Container (List Topic) -> List Topic
                topicList cont =
                    case (currentPage cont) of
                        Loaded topics ->
                            topics
                        _ ->
                            []
                newSlots =
                    case submsg of
                        ContainerCache.CreateNewContainer _ ->
                            let index =
                                    (Array.length newdata.arrayOfContainer) - 1
                                newContainer =
                                    (withDefault ContainerCache.defaultContainer (Array.get index newdata.arrayOfContainer))
                                newView =
                                    TopicsView "Topics" (topicList newContainer) index
                            in
                            slotFromTo model.slots Empty newView
                        ContainerCache.PageUpdate _ _ ->
                            let index =
                                    case (slotGet model.slots slotId) of
                                        TopicsView _ _ contId->
                                            contId
                                        _ ->
                                            -1
                                newContainer =
                                    (withDefault ContainerCache.defaultContainer (Array.get index newdata.arrayOfContainer))
                                newView =
                                    TopicsView "Topics" (topicList newContainer) index
                            in
                            slotChangeTo model.slots slotId newView
                        _ ->
                            let index =
                                    case (slotGet model.slots slotId) of
                                        TopicsView _ _ contId->
                                            contId
                                        _ ->
                                            -1
                                newContainer =
                                    (withDefault ContainerCache.defaultContainer (Array.get index newdata.arrayOfContainer))
                                newView =
                                    TopicsView "Topics" (topicList newContainer) index
                            in
                            slotChangeTo model.slots slotId newView
            in
            ({ model
                | containerTopicModel = newdata
                , slots = newSlots
                }
            , Platform.Cmd.map (ContainerCacheTopicMsg slotId) cmd )
        Mdl msgmdl ->
            (Material.update Mdl msgmdl model)
        _ ->
            ( model, Cmd.none)
