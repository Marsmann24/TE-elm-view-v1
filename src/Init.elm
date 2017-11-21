module Init exposing (init)

import Material
import Model exposing (..)

init : (Model, Cmd Msg)
init =
    ({ topics = List.map initTopic (List.range 0 3)
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
    , raised = -1
    , wordList = []
    , tabs = initTabs
    , currentTab = 0
    , settings = initSettings
    , slots =
        { s1 = TopicsView
        , s2 = Empty
        , s3 = Empty
        }
    , mdl = Material.model
    } , Cmd.none)

initSettings : Settings
initSettings =
    { showTopics = True
    , showArticles = True
    , showWordlist = False
    , showSaved = True
    , bottom = False
    , view2 = False
    , showSlotDialoge = False
    }

initTabs : List Tab
initTabs =
    [ PreviewTab
    , ArticleTab  "Article 0"
        { articleID = 1
        , rankedTopics = [0, 1, 2]
        , title = "Article 0"
        , date = "13.07.1999"
        , text = "Hallo, ich habe hier nur einen Dummytext."
        }
    ]

initArticle : Int -> Article
initArticle id =
    case id of
        0 ->
            { articleID = 0
            , rankedTopics = [0, 1, 2]
            , title = "Hallo"
            , date = "11.11.2011"
            , text = "Dieser Artikel ist der, der gerade an der Seite ausgewählt ist."
            }
        1 ->
            { articleID = 1
            , rankedTopics = [0, 1, 2]
            , title = "Article 0"
            , date = "13.07.1999"
            , text = "Hallo, ich habe hier nur einen Dummytext."
            }
        _ ->
            {articleID = id
            , rankedTopics = []
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
            , words = [ "Stuff", "Things", "Items"]
            }
        1 ->
            { topicID = id
            , topicName = "Other"
            , words = [ "Some", "Any", "Other"]
            }
        2 ->
            { topicID = 2
            , topicName = "Tryout"
            , words = [ "test", "time"]
            }
        _ ->
            { topicID = id
            , topicName = "Dummy"
            , words = [ "one", "two", "three"]
            }
