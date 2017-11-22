module Model exposing (..)

import Material
import Html exposing (Html)

type Msg
    = SelectTab Int
    | Raise Int
    | ChangeCurrentArticle Int Article
    | RemoveTopic Int
    | ShowTopics (List Topic)
    | HideTopics Int
    | ShowWordList (List String)
    | HideWordList Int
    | ShowArticles String
    | HideArticles Int
    | ChoseSlotDialog Int
    | UpdateSlot View Int
    | ToggleBottom
    | ToggleView2
    | ToggleShowSaved
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
    , wordList : List String        -- current word list
    , currentWord : String          -- current word
    , tabs : List Tab               -- all tabs
    , currentTab : Int              -- active tab
    , raised : Int                  -- ID of raised card
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
    , words : List String
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

type Tab
    = PreviewTab
    | ArticleTab String Article
    | ErrorTab String String

-- slots
type alias Slots =
    { s1 : View
    , s2 : View
    , s3 : View
    }

type View
    = WordlistView (List String)
    | TopicsView (List Topic)
    | ArticlesView (List Article)
    | Dialog
    | Empty

slotFromTo : Slots -> View -> View -> Slots
slotFromTo oldSlots from to =
    if (oldSlots.s1 == from)
        then
            slotChangeTo oldSlots 1 to
        else if (oldSlots.s2 == from)
            then
                slotChangeTo oldSlots 2 to
            else
                slotChangeTo oldSlots 3 to

slotChangeTo : Slots -> Int -> View -> Slots
slotChangeTo oldSlots id value =
    case id of
        1 ->
            { oldSlots | s1 = value}
        2 ->
            { oldSlots | s2 = value}
        3 ->
            { oldSlots | s3 = value}
        _ ->
            oldSlots

slotRemove : Slots -> Int -> Slots
slotRemove oldSlots id =
    case id of
        1 ->
            slotRemove (slotChangeTo oldSlots 1 oldSlots.s2) 2
        2 ->
            slotRemove (slotChangeTo oldSlots 2 oldSlots.s3) 3
        _ ->
            slotChangeTo oldSlots id Empty
