module Init exposing (init)

import Model exposing (..)
import Term exposing (Term)
import Topic exposing (Topic)
import Document exposing (Document, Doc)
import Request exposing (loadTopics)
import ContainerCache

import Material
import Array

init : (Model, Cmd Msg)
init =
    ({ result = initResult
    , topics = []
    , currentTopics = []
    , docs = []
    , currentDocument = { cardID = 0, document = initDocument 0}
    , terms = []
    , currentTerm = initTerm 0
    , tabs = initTabs
    , currentTab = 0
    , raised = -1
    , settings = initSettings
    , slots =
        { main =
            Array.fromList
                [ Empty
                , Empty
                , Empty
                ]
        , more = []
        }
    , containerTopicModel = ContainerCache.newContainerModel Array.empty 3 ContainerCache.defaultContainer
    , topicsContainer = 0
    , mdl = Material.model
    } , loadTopics)

initResult : List Searchresult
initResult =
    [ Termresult (initTerm 0)
    , Termresult (initTerm 1)
    , Termresult (initTerm 2)
    , Documentresult (initDoc 0)
    , Documentresult (initDoc 1)
    , Topicresult (initTopic 0)
    , Topicresult (initTopic 1)
    ]

initSettings : Settings
initSettings =
    { showTopics = True
    , showDocuments = True
    , showTerms = False
    , showSaved = True
    , bottom = False
    , view2 = True
    , showSlotDialoge = False
    , search = False
    , error = "Ok"
    , slotToDelete = -1
    }

initTabs : List Tab
initTabs =
    [-- PreviewTab
    ]

initDocument : Int -> Document
initDocument id =
    case id of
        _ ->
            { id = id
            , linkurl = "http://example.com/Docunment"
            , time_stamp = 0
            , title = "document"
            , fulltext = "This is a test document. Further content is not available."
            , search_test = "whatever"
            , frame_list = []
            , word_list =
                [
                { topic_id = 0
                , posintion_in_document = 0
                , term = "term"
                , parent_topic_ids = []
                }
                ]
            }

initDoc : Int -> Doc
initDoc id =
    case id of
        _ ->
            { document_id = id
            , topic_id = 0
            , document_count = "0"
            , keyword_snippet = "key snippet"
            , keyword_title = "key title"
            , top_topic = [0]
            , linkurl = "http://example.com/Docunment"
            , time_stamp = 0
            , title = "title"
            , snippet = "snippet"
            }

initTopic : Int -> Topic
initTopic id =
    case id of
        _ ->
            { id = id
            , hirarchical_topic =
                { start = 0
                , end = 0
                , depth = 0
                , cluster = Just "0;1;1;1"
                }
            , color_topic = "#ffffff"
            , top_terms = [initTerm 0]
            }

initTerm : Int -> Term
initTerm id =
    case id of
        _ ->
            { id = id
            , name = "term"
            , wordtype = Just 0
            , count = Just 0
            }
