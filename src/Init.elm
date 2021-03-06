module Init exposing (init, initSettings)

import Model exposing (..)
import Topic exposing (Topic, defaultTopic)
import Term exposing (Term, defaultTerm)
import Document exposing (Doc, defaultDoc, Document, defaultDocument)
import Request exposing (loadTopics)
import ContainerCache

import Material
import Array

init : (Model, Cmd Msg)
init =
    ({ topics = []
    , currentTopics = []
    , docs = []
    , currentDocument = { cardID = 0, document = initDocument 0}
    , terms = []
    , currentTerm = initTerm 0
    , tabs = initTabs
    , currentTab = 0
    , raised = -1
    , selectedMsg = None
    , settings = initSettings
    --, result = initResult
    , slots =
        { left = []
        , focus = Array.initialize 3 (always Empty)
        , right = []
        , null = Empty
        }
        --{ main =
        --    Array.fromList
        --        [ Empty
        --        , Empty
        --        , Empty
        --        ]
        --, more = []
        --}
    , containerTopicModel = ContainerCache.newContainerModel Array.empty 3 ContainerCache.defaultContainer
    , topicsContainer = 0
    , mdl = Material.model
    } , (loadTopics 0))

--initResult : List Searchresult
--initResult =
--    [ Termresult (initTerm 0)
--    , Termresult (initTerm 1)
--    , Termresult (initTerm 2)
--    , Documentresult (initDoc 0)
--    , Documentresult (initDoc 1)
--    , Topicresult (initTopic 0)
--    , Topicresult (initTopic 1)
--    ]

initSettings : Settings
initSettings =
    { error = ""
    --, showTopics = True
    --, showDocuments = True
    --, showTerms = False
    , showSaved = False
    , mobile = False
    , docview = False
    , bottom = False
    , view2 = False
    , parent = Noparent
    , showRelevance = True
    , showSlotDialoge = False
    , search = False
    , search4 = ""
    , searchResult = TopicResult []
    , slotToDelete = -1
    , selectedItem = (-1, -1)
    }

initTabs : List Tab
initTabs =
    [-- PreviewTab
    ]

initDocument : Int -> Document
initDocument id =
    { defaultDocument | id = id}

initDoc : Int -> Doc
initDoc id =
    { defaultDoc | id = id}

initTopic : Int -> Topic
initTopic id =
    { defaultTopic | id = id}

initTerm : Int -> Term
initTerm id =
    { defaultTerm | id = id}
