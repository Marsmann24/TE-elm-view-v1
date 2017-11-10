module Model exposing (..)

import Material

type alias Model =
    { topics : List Topic           -- all topics
    , currentTopics : List Topic    -- list of topics for the ranking
    , articles : List Article       -- ranked articles
    , currentArticle :              -- active card and preview article
        { cardID : Int
        , article : Article
        }
    , raised : Int                  -- ID of raised card
    , row : Bool                    -- if word list is shown
    , wordList : List String        -- current word list
    , tabs : List Tab               -- all tabs
    , currentTab : Int              -- active tab
    , mdl : Material.Model
    }

type alias Topic =
    { topicID : Int
    , topicName : String
    , words : List String
    }

type alias Article =
    { articleID : Int
    , rankedTopics : List Int
    , title : String
    , date : String
    , text : String
    }

type Tab
    = PreviewTab
    | ArticleTab String Article
    | ErrorTab String String
type Msg
    = SelectTab Int
    | Raise Int
    | ChangeCurrentArticle Int Article
    | RemoveTopic Int
    | ShowWordList (List String)
    | HideWordList
    | None
    | Mdl (Material.Msg Msg)
