module Model exposing (..)

import Material
import Html exposing (Html)

type Msg
    = SelectTab Int
    | Raise Int
    | ChangeCurrentArticle Int Article
    | RemoveTopic Int
    | ShowWordList (List String)
    | HideWordList Int
    | ToggleBottom
    | ToggleView2
    | ToggleShowSaved
    | ToggleShowArticles
    | ChoseSlotDialoge
    | Mdl (Material.Msg Msg)
    | None -- zum Testen, damit update _ -> immer haben kann

type alias Model =
    { topics : List Topic           -- all topics
    , currentTopics : List Topic    -- list of topics for the ranking
    , articles : List Article       -- ranked articles
    , currentArticle :              -- active card and preview article
        { cardID : Int
        , article : Article
        }
    , raised : Int                  -- ID of raised card
    , wordList : List String        -- current word list
    , tabs : List Tab               -- all tabs
    , currentTab : Int              -- active tab
    , settings : Settings           -- which views are shown
    , slots : Slots
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

type alias Settings =
    { showTopics : Bool
    , showArticles : Bool
    , showWordlist : Bool
    , showSaved : Bool
    , bottom : Bool
    , view2 : Bool
    , showSlotDialoge : Bool
    }

type alias Slots =
    { s1 : View
    , s2 : View
    , s3 : View
    }

type View
    = WordlistView
    | TopicsView
    | ArticlesView
    | Empty

type Tab
    = PreviewTab
    | ArticleTab String Article
    | ErrorTab String String
