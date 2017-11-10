module Init exposing (init)

import Material
import Model exposing (..)

init : (Model, Cmd Msg)
init =
    ( { topics =
          [ { topicID = 0
            , topicName = "Stuff"
            , words = [ "Stuff", "Things", "Items"]
            }
          , { topicID = 1
            , topicName = "Other"
            , words = [ "Some", "Any", "Other"]
            }
          ]
      , currentTopics = [ { topicID = 0, topicName = "Dummy", words = [ "one", "two", "three"]}
                        , { topicID = 5, topicName = "Tryout", words = [ "test", "time"]}]
      , articles =
          [ { articleID = 0
            , rankedTopics = [0, 1, 2]
            , title = "Hallo"
            , date = "11.11.2011"
            , text = "Dieser Artikel ist der, der gerade an der Seite ausgewählt ist."
            }
          , { articleID = 1
            , rankedTopics = [0, 1, 2]
            , title = "Article 0"
            , date = "13.07.1999"
            , text = "Hallo, ich habe hier nur einen Dummytext."
            }
          ]
      , currentArticle =
          { cardID = 0
          , article =
              { articleID = 0
              , rankedTopics = [0, 1, 2]
              , title = "Hallo"
              , date = "11.11.2011"
              , text = "Dieser Artikel ist der, der gerade an der Seite ausgewählt ist."
              }
          }
      , raised = -1
      , row = False
      , wordList = []
      , tabs =
          [ PreviewTab
          , ArticleTab  "Article 0"
                        { articleID = 1
                        , rankedTopics = [0, 1, 2]
                        , title = "Article 0"
                        , date = "13.07.1999"
                        , text = "Hallo, ich habe hier nur einen Dummytext."
                        }
          ]
      , currentTab = 0
      , mdl = Material.model
      } , Cmd.none)
