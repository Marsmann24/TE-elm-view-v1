module Model exposing (..)

import Material
import Html exposing (Html)
import Array exposing (Array)

type Msg
    = Search String
    | Found View
    | SelectTab Int
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
    { result : List Searchresult          -- search result
    , topics : List Topic           -- all topics
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
    , search : Bool
    }

type Tab
    = PreviewTab
    | ArticleTab String Article
    | ErrorTab String String

type Searchresult
    = Topicresult Topic
    | Wordresult String
    | Articleresult Article

--Slots

type View
    = WordlistView (List String)
    | TopicsView (List Topic)
    | ArticlesView (List Article)
    | Dialog
    | Empty
    | ErrorSlot

type alias Slots =
    { main : Array View
    , more : List View
    }

slotFromTo : Slots -> View -> View -> Slots
slotFromTo slots from to =
    let findSlot = slotGetFirstId slots from
    in
    if (findSlot == -1)
    then
        slotChangeTo
            (slotChangeTo
                (slotChangeTo
                    { slots | more = ((slotGet slots 0)::slots.more)}
                    0 (slotGet slots 1))
                1 (slotGet slots 2))
            2 to
    else slotChangeTo slots findSlot to

slotGetFirstId : Slots -> View -> Int
slotGetFirstId slots view =
    slotGetFirstIdSince slots view 0

slotGetFirstIdSince : Slots -> View -> Int -> Int
slotGetFirstIdSince slots view id =
    let getView =
        case (Array.get id slots.main) of
            Just a ->
                a
            Nothing ->
                ErrorSlot
    in
    if (getView == ErrorSlot)
    then -1
    else if (getView == view)
        then id
        else (slotGetFirstIdSince slots view (id + 1))

slotGet : Slots -> Int -> View
slotGet slots slotId =
    case (Array.get slotId slots.main) of
        Just a ->
            a
        Nothing ->
            ErrorSlot

slotChangeTo : Slots -> Int -> View -> Slots
slotChangeTo oldSlots id value =
    { oldSlots | main = Array.set id value oldSlots.main}

slotRemove : Slots -> Int -> Slots
slotRemove slots id =
    let moreHead =
            case (List.head slots.more) of
                Just a ->
                    a
                Nothing ->
                    Empty
        moreTail =
            case (List.tail slots.more) of
                Just a ->
                    a
                Nothing ->
                    []
        nextId =
            if (moreHead == Empty)
            then id + 1
            else id - 1
        nextSlot = slotGet slots nextId
    in
    if (nextSlot == ErrorSlot)
    then
        slotChangeTo { slots | more = moreTail} id moreHead
    else
        slotRemove (slotChangeTo slots id nextSlot) nextId
