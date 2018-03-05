module Update exposing (update)

import Model exposing (..)
import Topic exposing (Topic)
import Term exposing (Term)
import Document exposing (Doc, Document)

import Material
import Delay
import Time
import ContainerCache exposing (Page(..))
import Platform.Cmd
import Maybe exposing (withDefault)
import Array
import Set

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
                    | settings =
                        { oldSettings
                            | search = True
                            , search4 = term
                        }
                    --, result = List.filter (\x -> (x == term)) []
                }, Cmd.none)
        --Found view ->
        --        let oldSettings = model.settings
        --            oldSlots = model.slots
        --        in
        --        ({ model
        --            | settings = { oldSettings
        --                            | search = False
        --                            , search4 = ""
        --                        }
        --            , slots = slotFromTo oldSlots Empty view
        --            }
        --        , Cmd.none)
        DeleteSlot slotId ->
            let oldSettings = model.settings
            in
            ({ model
                | settings = { oldSettings | slotToDelete = slotId}
                }
            , Delay.after 200 Time.millisecond (RemoveSlot slotId))
        RemoveSlot slotId ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            ({ model
                | settings = { oldSettings | slotToDelete =-1}
                , slots =  slotRemove oldSlots slotId
                }
            , Cmd.none)
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
--        ShowTopics topics ->
--            let oldSettings = model.settings
--                oldSlots = model.slots
--                topicsContainer = model.topicsContainer
--            in
--            ({ model
--                | settings = { oldSettings | showTopics = True}
--                , slots = slotFromTo oldSlots Empty (TopicsView "Topics" topics topicsContainer)
--                }
--            , Cmd.none)
--        HideTopics slotId ->
--            let oldSettings = model.settings
--                oldSlots = model.slots
--            in
--            ({ model
--                | settings = { oldSettings | showTopics = False, slotToDelete =-1}
--                , slots =  slotRemove oldSlots slotId
--                }
--            , Cmd.none)
--        ShowTerms terms ->
--            let oldSettings = model.settings
--                oldSlots = model.slots
--            in
--            ({ model
--                | settings = { oldSettings | showTerms = True}
--        --        , termList = terms
--                , slots = slotFromTo oldSlots Empty (TermsView "Terms" terms)
--                }
--            , Cmd.none)
--        HideTerms slotId ->
--            let oldSettings = model.settings
--                oldSlots = model.slots
--            in
--            ({ model
--                | settings = { oldSettings | showTerms = False, slotToDelete =-1}
--                , slots =  slotRemove oldSlots slotId
--                }
--            , Cmd.none)
--        ShowDocuments documents ->
--            let oldSettings = model.settings
--                oldSlots = model.slots
--                docs = model.docs
--                contains term document = List.member term document.terms
--            in
--            ({ model
--                | settings = { oldSettings | showDocuments = True}
--                , slots = slotFromTo oldSlots Empty (DocumentsView "Documents" docs)
--                }
--            , Cmd.none)
--        HideDocuments slotId ->
--            let oldSettings = model.settings
--                oldSlots = model.slots
--            in
--            ({ model
--                | settings = { oldSettings | showDocuments = False, slotToDelete =-1}
--                , slots =  slotRemove oldSlots slotId
--                }
--            , Cmd.none)
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
            ({ model | slots = slotChangeTo oldSlots slotId view}, Cmd.none)
        NewTopics name result ->
            let oldSettings = model.settings
                oldSlots = model.slots
                topicsContainer = model.topicsContainer
            in
            case result of
                Ok newTopics ->
                    ({ model
                        | slots = slotFromTo oldSlots Empty (TopicsView name newTopics topicsContainer)
                        , topics = newTopics
                        --, settings = { oldSettings | showTopics = True}
                    }, Cmd.none)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, Cmd.none)
        NewTerms name result ->
            let oldSettings = model.settings
                oldSlots = model.slots

            in
            case result of
                Ok newTerms ->
                    ({ model
                        | slots = slotFromTo oldSlots Empty (TermsView name newTerms)
                        , terms = newTerms
                        --, settings = { oldSettings | showTerms = True}
                    }, Cmd.none)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, Cmd.none)
        NewDocs name result ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            case result of
                Ok newDocs ->
                    ({ model
                        | slots = slotFromTo oldSlots Empty (DocumentsView name newDocs)
                        , docs = newDocs
                        --,settings = { oldSettings | showDocuments = True}
                    }, Cmd.none)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, Cmd.none)
        NewDocTokens name result ->
            let oldSettings = model.settings
                oldSlots = model.slots
                allTerms = model.terms
            in
            case result of
                Ok document ->
                    ({ model
                        | slots = slotFromTo oldSlots Empty (TermsView name (Document.documentTerms document allTerms))
                        --,settings = { oldSettings | showTerms = True}
                    }, Cmd.none)
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
        NewFrames name result ->
            (model, Cmd.none)
        NewTermTopics termName result ->
            let oldSettings = model.settings
                topicsContainer = model.topicsContainer
                oldSlots = model.slots
                fetchTerm : List Term -> Maybe Term
                fetchTerm terms =
                    List.head (List.filter (\x -> x.name == termName) terms)
                maybeTerm2TopicList : Maybe Term -> List Int
                maybeTerm2TopicList terms =
                    case terms of
                        Just term ->
                            term.top_topic
                        Nothing ->
                            []
                isMember : List Term -> Topic -> Bool
                isMember terms topics=
                    List.member
                        topics.id
                        (maybeTerm2TopicList
                            (fetchTerm terms)
                        )
                newTopics : List Term -> List Topic
                newTopics terms =
                    List.filter
                        (isMember terms)
                        model.topics
            in
            case result of
                Ok termList ->
                    ({ model
                        | slots = slotFromTo oldSlots Empty (TopicsView ("Topics with " ++ termName) (newTopics termList) topicsContainer)
                        --,settings = { oldSettings | showTopics = True}
                    }, Cmd.none)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, Cmd.none)
        NewSearchTopics name result ->
            let oldSettings = model.settings
                topicsContainer = model.topicsContainer
                oldSlots = model.slots
                concatTopTopics : List Term -> List Int
                concatTopTopics terms =
                    Set.toList
                        (Set.fromList
                            (List.concat
                                (List.map .top_topic terms)
                            )
                        )
                isMember : List Term -> Topic -> Bool
                isMember terms topics=
                    List.member
                        topics.id
                        (List.map .id terms)
                newTopics : List Term -> List Topic
                newTopics terms =
                    List.filter
                        (isMember terms)
                        model.topics
            in
            case result of
                Ok termList ->
                    ({ model
                        | slots = slotFromTo oldSlots Empty (TopicsView name (newTopics termList) topicsContainer)
                        , settings =
                            { oldSettings
                                | search = False
                                , search4 = ""
                                , error = toString concatTopTopics
                            --    , showTopics = True
                            }
                    }, Cmd.none)
                Err err ->
                    ({ model
                        | settings =
                            { oldSettings
                                | search = False
                                , search4 = ""
                                , error = toString err
                            }
                    }, Cmd.none)
        NewSearchTerms name result ->
            let oldSettings = model.settings
                oldSlots = model.slots
            in
            case result of
                Ok termList ->
                    ({ model
                        | slots = slotFromTo oldSlots Empty (TermsView name termList)
                        , settings =
                            { oldSettings
                                | search = False
                                , search4 = ""
                            --    , showTerms = True
                            }
                    }, Cmd.none)
                Err err ->
                    ({ model
                        | settings =
                            { oldSettings
                                | search = False
                                , search4 = ""
                                , error = toString err
                            }
                    }, Cmd.none)
        NewSearchDocs name result ->
            let oldSettings = model.settings
                oldSlots = model.slots

            in
            case result of
                Ok docList ->
                    ({ model
                        | slots = slotFromTo oldSlots Empty (DocumentsView name docList)
                        , settings =
                            { oldSettings
                                | search = False
                                , search4 = ""
                            --    , showDocuments = True
                            }
                    }, Cmd.none)
                Err err ->
                    ({ model
                        | settings =
                            { oldSettings
                                | search = False
                                , search4 = ""
                                , error = toString err
                            }
                    }, Cmd.none)
        ExecCmd cmd ->
            (model, cmd)
        SelectAction defMsg ->
            ({ model | selectedMsg = defMsg}, Cmd.none)
        ExecuteActionIfNone defMsg ->
            let newMsg =
                    if (model.selectedMsg == None)
                    then defMsg
                    else model.selectedMsg
            in
            update newMsg { model | selectedMsg = None}
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
