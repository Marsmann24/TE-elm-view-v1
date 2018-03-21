module Update exposing (update)

import Model exposing (..)
import Init exposing (initSettings)
import Topic exposing (Topic)
import Term exposing (Term)
import Document exposing (Doc, Document)
import Request
import Slots

import Material
import Dispatch
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
        Toggle settings ->
            ({ model | settings = settings}, Cmd.none)
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
        SelectItem itemId ->
            let oldSettings = model.settings
            in
            ({ model
                | settings =
                    { oldSettings | selectedItem = itemId}
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
        Search searchterm ->
            let oldSettings = model.settings
            in
            if not (searchterm == "")
            then
                ({ model
                    | settings =
                        { oldSettings
                            | search = not (searchterm == "")
                            , search4 = searchterm
                        }
                }, Request.loadSearchTerms searchterm)
            else
                ( model, Cmd.none)
        AdvancedSearch string ->
            let oldSettings = model.settings
                searchterm = String.toLower string
            in
            if not (searchterm == "")
            then
                let request =
                        (if (String.startsWith "topic:" searchterm)
                        then
                            let search4topic = (String.dropLeft 6 searchterm)
                            in
                            (Request.loadSearchTopics search4topic)
                        else (if (String.startsWith "term:" searchterm)
                        then
                            let search4term = (String.dropLeft 5 searchterm)
                            in
                            (Request.loadSearchTerms search4term)
                        else (if (String.startsWith "document:" searchterm)
                        then
                            let search4doc = (String.dropLeft 9 searchterm)
                                docId = String.toInt search4doc
                            in
                            case docId of
                                Ok id ->
                                    let doc = Document.defaultDoc
                                    in
                                    (Request.loadDoc { doc | id = id})
                                Err _ ->
                                    (Request.loadSearchDocs search4doc False "RELEVANCE")
                        else
                            (Request.loadSearchTerms searchterm)
                        )))
                in
                ({ model
                    | settings =
                        { oldSettings
                            | search = False
                            , search4 = searchterm
                        }
                }, request)
            else
                ( model, Cmd.none)
        ResetSettings ->
            ({ model | settings = initSettings}, Cmd.none)
        Found view ->
                let oldSettings = model.settings
                in
                ({ model
                    | settings = { oldSettings
                                    | search = False
                                    , search4 = ""
                                }
                    , slots = Slots.insertEnd model.slots view
                    }
                , Cmd.none)
        DeleteSlot slotId ->
            let oldSettings = model.settings
            in
            ({ model
                | settings = { oldSettings | slotToDelete = slotId}
                }
            , Delay.after 200 Time.millisecond (RemoveSlot slotId))
        RemoveSlot slotId ->
            let oldSettings = model.settings
            in
            ({ model
                | settings = { oldSettings | slotToDelete =-1}
                , slots =  Slots.removeAt model.slots slotId
                }
            , Cmd.none)
        MoveLeft ->
            ({ model
                | slots = (Slots.moveLeft model.slots)
            }, Cmd.none)
        MoveRight ->
            ({ model
                | slots = (Slots.moveRight model.slots)
            }, Cmd.none)
        RemoveSlotFromOther id ->
            ({ model
                | slots = model.slots -- slotRemoveMore model.slots id
            }, Cmd.none)
        SlotToLastFromOther id ->
            ({ model
                | slots = model.slots --slotMove2EndFromMore model.slots id
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
            in
            ({ model
                | settings = { oldSettings | showSlotDialoge = True}
                , slots = Slots.insertEnd model.slots Dialog --slotChangeTo oldSlots slotId Dialog
                }
            , Cmd.none)
        UpdateSlot view slotId ->
            ({ model
                | slots = Slots.setAt model.slots view slotId
                                        --slotChangeTo oldSlots slotId view
            }, Cmd.none)
        NewTopics name result ->
            let oldSettings = model.settings
                topicsContainer = model.topicsContainer
            in
            case result of
                Ok newTopics ->
                    ({ model
                        | slots = Slots.insertEnd model.slots (TopicsView name newTopics topicsContainer)
                                        --slotFromTo oldSlots Empty (TopicsView name newTopics topicsContainer)
                        , topics = newTopics
                        , settings =
                            { oldSettings
                                | error = ""
                                --,  showTopics = True
                            }
                    }, Cmd.none)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, Cmd.none)
        NewTerms name result ->
            let oldSettings = model.settings
            in
            case result of
                Ok newTerms ->
                    ({ model
                        | slots = Slots.insertEnd model.slots (TermsView name newTerms)
                                        --slotFromTo oldSlots Empty (TermsView name newTerms)
                        , terms = newTerms
                        , settings =
                            { oldSettings
                                | error = ""
                                --, showTerms = True
                            }
                    }, Cmd.none)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, Cmd.none)
        NewDocs name result ->
            let oldSettings = model.settings
            in
            case result of
                Ok newDocs ->
                    ({ model
                        | slots = Slots.insertEnd model.slots (DocumentsView name newDocs)
                                        --slotFromTo oldSlots Empty (DocumentsView name newDocs)
                        , docs = newDocs
                        ,settings =
                            { oldSettings
                                | error = ""
                                --, showDocuments = True
                            }
                    }, Cmd.none)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, Cmd.none)
        NewDocTokens name result ->
            let oldSettings = model.settings
                allTerms = model.terms
            in
            case result of
                Ok document ->
                    ({ model
                        | slots = Slots.insertEnd model.slots (TermsView name (Document.documentTerms document allTerms))
                                        --slotFromTo oldSlots Empty (TermsView name (Document.documentTerms document allTerms))
                        ,settings =
                            { oldSettings
                                | error = ""
                                --, showTerms = True
                            }
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
                        , currentTab = tabNumber
                        , settings = { oldSettings | error = ""}
                    }, Cmd.none)
                Err err ->
                    ({ model | settings = { oldSettings | error = "Document not found"}}, Cmd.none)
        NewFrames name result ->
            (model, Cmd.none)
        NewTermTopics termName result ->
            let oldSettings = model.settings
                topicsContainer = model.topicsContainer
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
                        | slots = Slots.insertEnd model.slots (TopicsView ("Topics with " ++ termName) (newTopics termList) topicsContainer)
                                        --slotFromTo oldSlots Empty (TopicsView ("Topics with " ++ termName) (newTopics termList) topicsContainer)
                        , settings =
                            { oldSettings | error = ""
                            --, showTopics = True
                            }
                    }, Cmd.none)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, Cmd.none)
        NewSearchTopics name result ->
            let oldSettings = model.settings
                topicsContainer = model.topicsContainer
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
                        (concatTopTopics terms)
                newTopics : List Term -> List Topic
                newTopics terms =
                    List.filter
                        (isMember terms)
                        model.topics
            in
            case result of
                Ok termList ->
                    ({ model
                        | slots = Slots.insertEnd model.slots (TopicsView name (newTopics termList) topicsContainer)
                                        --slotFromTo oldSlots Empty (TopicsView name (newTopics termList) topicsContainer)
                        , settings =
                            { oldSettings
                                | search = False
                                , error = ""
                            --    , search4 = ""
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
            in
            case result of
                Ok termList ->
                    ({ model
                        --| slots = slotFromTo oldSlots Empty (TermsView name termList)
                        | settings =
                            { oldSettings
                                | searchResult = TermResult termList
                                , error = ""
                            --    , search4 = ""
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
            in
            case result of
                Ok docList ->
                    ({ model
                        | slots = Slots.insertEnd model.slots (DocumentsView name docList)
                                        --slotFromTo oldSlots Empty (DocumentsView name docList)
                        , settings =
                            { oldSettings
                                | search = False
                                , error = ""
                            --    , search4 = ""
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
            let oldSettings = model.settings
            in
            ({ model
                | settings =
                    { oldSettings | selectedItem = (-1, -1)}
            }, cmd)
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
                            Slots.insertEnd model.slots newView
                            --slotFromTo model.slots Empty newView
                        ContainerCache.PageUpdate _ _ ->
                            let index =
                                    case (Slots.getById model.slots slotId) of
                                        TopicsView _ _ contId->
                                            contId
                                        _ ->
                                            -1
                                newContainer =
                                    (withDefault ContainerCache.defaultContainer (Array.get index newdata.arrayOfContainer))
                                newView =
                                    TopicsView "Topics" (topicList newContainer) index
                            in
                            Slots.setAt model.slots newView slotId
                            --slotChangeTo model.slots slotId newView
                        _ ->
                            let index =
                                    case (Slots.getById model.slots slotId) of
                                        TopicsView _ _ contId->
                                            contId
                                        _ ->
                                            -1
                                newContainer =
                                    (withDefault ContainerCache.defaultContainer (Array.get index newdata.arrayOfContainer))
                                newView =
                                    TopicsView "Topics" (topicList newContainer) index
                            in
                            Slots.setAt model.slots newView slotId
                            --slotChangeTo model.slots slotId newView
            in
            ({ model
                | containerTopicModel = newdata
                , slots = newSlots
                }
            , Platform.Cmd.map (ContainerCacheTopicMsg slotId) cmd )
        Batch msg_ ->
            model ! [ Dispatch.forward msg_ ]
        Mdl msgmdl ->
            (Material.update Mdl msgmdl model)
        _ ->
            ( model, Cmd.none)
