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

--Slots

type View
    = WordlistView (List String)
    | TopicsView (List Topic)
    | ArticlesView (List Article)
    | Dialog
    | Empty
    | ErrorSlot

type alias Slots =
    { s1 : View
    , s2 : View
    , s3 : View
    , more : List View
    }

slotFromTo : Slots -> View -> View -> Slots
slotFromTo oldSlots from to =
    case (slotGetFirstId oldSlots from) of
        1 ->
            slotChangeTo oldSlots 1 to
        2 ->
            slotChangeTo oldSlots 2 to
        3 ->
            slotChangeTo oldSlots 3 to
        _ ->
            let slots = oldSlots
            in
            slotChangeTo
                (slotChangeTo
                    (slotChangeTo
                        { oldSlots | more = (slots.s1::slots.more)}
                        1 slots.s2)
                    2 slots.s3)
                3 to

slotGetFirstId : Slots -> View -> Int
slotGetFirstId slots view =
    if (slots.s1 == view)
    then 1
    else
        if (slots.s2 == view)
        then 2
        else
            if (slots.s3 == view)
            then 3
            else 0

slotGet : Slots -> Int -> View
slotGet slots slotId =
    case slotId of
        1 ->
            slots.s1
        2 ->
            slots.s2
        3 ->
            slots.s3
        _ ->
            ErrorSlot

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
slotRemove slots id =
    if (slots.more == [])
    then
        case id of
            1 ->
                slotRemove (slotChangeTo slots 1 slots.s2) 2
            2 ->
                slotRemove (slotChangeTo slots 2 slots.s3) 3
            3 ->
                slotChangeTo slots id Empty
            _ ->
                slots
    else
        case id of
            1 ->
                let moreHead =
                        case (List.head slots.more) of
                            Just a ->
                                a
                            Nothing ->
                                ErrorSlot
                    moreTail =
                        case (List.tail slots.more) of
                            Just a ->
                                a
                            Nothing ->
                                [ErrorSlot]
                in
                slotChangeTo { slots | more = moreTail} 1 moreHead
            2 ->
                slotRemove (slotChangeTo slots 2 slots.s1) 1
            3 ->
                slotRemove (slotChangeTo slots 3 slots.s2) 2
            _ ->
                slots
