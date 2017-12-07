module Init exposing (init)

import Material
import Model exposing (..)
import Array

init : (Model, Cmd Msg)
init =
    ({ result = initResult
    , topics = List.map initTopic (List.range 0 8)
    , currentTopics =
        [ initTopic 0
        , initTopic 5
        ]
    , articles =
        [ initArticle 0
        , initArticle 1
        ]
    , currentArticle =
        { cardID = 0
        , article = initArticle 0
        }
    , wordList = []
    , currentWord = ""
    , tabs = initTabs
    , currentTab = 0
    , raised = -1
    , settings = initSettings
    , slots =
        { main =
            Array.fromList
                [ TopicsView (List.map initTopic (List.range 0 8))
                , Empty
                , Empty
                ]
        , more = []
        }
    , mdl = Material.model
    } , Cmd.none)

initResult : List Searchresult
initResult =
    [ Wordresult "test"
    , Wordresult "time"
    , Wordresult "Stuff"
    , Articleresult (initArticle 0)
    , Articleresult (initArticle 1)
    , Topicresult (initTopic 0)
    , Topicresult (initTopic 1)
    ]

initSettings : Settings
initSettings =
    { showTopics = True
    , showArticles = True
    , showWordlist = False
    , showSaved = True
    , bottom = False
    , view2 = True
    , showSlotDialoge = False
    , search = False
    }

initTabs : List Tab
initTabs =
    [ PreviewTab
    , ArticleTab  "Article 0" (initArticle 1)
    ]

initArticle : Int -> Article
initArticle id =
    case id of
        0 ->
            { articleID = 0
            , rankedTopics = [0, 1, 2]
            , words = ["Some", "Any", "Stuff"]
            , title = "Hallo"
            , date = "11.11.2011"
            , text = "Dieser Artikel ist der, der gerade an der Seite ausgewählt ist."
            }
        1 ->
            { articleID = 1
            , rankedTopics = [0, 1, 2]
            , words = ["two", "time", "Item"]
            , title = "Article 0"
            , date = "13.07.1999"
            , text = "Hallo, ich habe hier nur einen Dummytext."
            }
        _ ->
            {articleID = id
            , rankedTopics = []
            , words = ["test"]
            , title = "0"
            , date = "0.0.0"
            , text = ""
            }

initTopic : Int -> Topic
initTopic id =
    case id of
        0 ->
            { topicID = id
            , topicName = "Stuff"
            , words = [ "Stuff", "Things", "Items", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff"]
            }
        1 ->
            { topicID = id
            , topicName = "Other"
            , words = [ "Some", "Any", "Other", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff"]
            }
        2 ->
            { topicID = 2
            , topicName = "Tryout"
            , words = [ "test", "time", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff"]
            }
        _ ->
            { topicID = id
            , topicName = "Dummy"
            , words = [ "one", "two", "three", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff", "Stuff"]
            }
