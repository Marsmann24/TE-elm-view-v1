module Model exposing (..)

import Term exposing (..)
import Topic exposing (..)
import Document exposing (..)

import ContainerCache
import Material
import Material.Options exposing (Property, cs)
import Material.Color as Color
import Http
import Html exposing (Html)
import Array exposing (Array)
import Maybe exposing (Maybe, withDefault)

type Msg
    = Search String
    --| Found View
    | SelectTab Int
    | CloseTab
    | Raise Int
    | ChangeCurrentDoc Int Doc
    | RemoveTopic Int
    | DeleteSlot Int
    | RemoveSlot Int
    | RemoveSlotFromOther Int
    | SlotToLastFromOther Int
    --| ShowTopics (List Topic)
    --| HideTopics Int
    --| ShowTerms (List Term)
    --| HideTerms Int
    --| ShowDocuments (List Doc)
    --| HideDocuments Int
    | ChoseSlotDialog Int
    | UpdateSlot View Int
    | ToggleBottom
    | ToggleView2
    | ToggleShowSaved
    | NewTopics String (Result Http.Error (List Topic))
    | NewDocument (Result Http.Error Document)
    | NewDocs String (Result Http.Error (List Doc))
    | NewDocTokens String (Result Http.Error Document)
    | NewTerms String (Result Http.Error (List Term))
    | NewFrames String (Result Http.Error (List Term))
    | NewTermTopics String (Result Http.Error (List Term))
    | NewSearchTopics String (Result Http.Error (List Term))
    | NewSearchTerms String (Result Http.Error (List Term))
    | NewSearchDocs String (Result Http.Error (List Doc))
    | ExecCmd (Cmd Msg)
    | SelectAction Msg
    | ExecuteActionIfNone Msg
    | ContainerCacheTopicMsg Int (ContainerCache.ContainerModelMsg (List Topic))
    | Batch (List Msg)
    | Mdl (Material.Msg Msg)
    | None -- zum Testen, damit update immer einen "_ ->"-Zweig haben kann

-- Command type for Request
--type Command
--    = GetTopics
--    | GetDoc
--    | GetBestDocs
--    | GetBestTerms
--    | GetTerms
--    | GetBestFrames

type alias Model =
    { topics : List Topic           -- all topics
    , currentTopics : List Topic    -- list of topics for the ranking
    , docs : List Doc                -- ranked articles
    , currentDocument :              -- active card and preview article
        { cardID : Int
        , document : Document
        }
    , terms : List Term             -- current term list
    , currentTerm : Term            -- current term
    , tabs : List Tab               -- all tabs
    , currentTab : Int              -- active tab
    , raised : Int                  -- ID of raised card
    , selectedMsg : Msg             -- Msg to execute onMouseUp
    , settings : Settings           -- which views are shown
    --, result : List Searchresult    -- search result
    , slots : Slots
    , containerTopicModel : ContainerCache.ContainerModel (List Topic)
    , topicsContainer : Int
    , mdl : Material.Model
    }

--type alias Topic =
--    { topicID : Int
--    , topicName : String
--    , terms : List String
--    }

--termInTopic : String -> Topic -> Bool
--termInTopic term topic =
--    List.member term topic.terms

--topicIDToTopic : List Topic -> Int -> Topic
--topicIDToTopic topics id =
--    withDefault
--        { topicID = id
--        , topicName = "ERROR"
--        , terms = []
--        }
--        (List.head (List.filter (\x -> x.topicID == id) topics))

--type alias Document =
--    { articleID : Int
--    , rankedTopics : List Int
--    , terms : List String
--    , title : String
--    , date : String
--    , text : String
--    }

--topicInDocument : Topic -> Document -> Bool
--topicInDocument topic article =
--    List.member topic.topicID article.rankedTopics

--termInDocument : String -> Document -> Bool
--termInDocument term article =
--    List.member term article.terms

type alias Settings =
    { error : String
    --, showTopics : Bool
    --, showDocuments : Bool
    --, showTerms : Bool
    , showSaved : Bool
    , bottom : Bool
    , view2 : Bool
    , showSlotDialoge : Bool
    , search : Bool
    , search4 : String
    , slotToDelete : Int
    }

type Tab
    = PreviewTab
    | DocumentTab String Document
    | ErrorTab String String

--type Searchresult
--    = Topicresult Topic
--    | Termresult Term
--    | Documentresult Doc

--Slots

type View
    = TermsView String (List Term)
    -- WordlistView (List String)
    | TopicsView String (List Topic) Int
    --| TopicsView (ContainerCache.Container (List Topic))
    --| DocumentsView (List Document)
    --| DocumentsView (ContainerCache.Container (List Doc))
    | DocumentsView String (List Doc)
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

slotsCount : Slots -> Int
slotsCount slots =
    let firstEmpty =
            slotGetFirstId slots Empty
    in
    if (firstEmpty < 0)
    then Array.length slots.main
    else firstEmpty

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
    withDefault ErrorSlot (Array.get slotId slots.main)

slotChangeTo : Slots -> Int -> View -> Slots
slotChangeTo oldSlots id value =
    { oldSlots | main = Array.set id value oldSlots.main}

slotRemove : Slots -> Int -> Slots
slotRemove slots removeId =
    let
        nextResult : Int -> View -> View
        nextResult id view =
            if ((nextSlot id) == ErrorSlot)
            then moreHead
            else if ((id * dir) >= (removeId * dir))
                then nextSlot id
                else view
        nextSlot : Int -> View
        nextSlot id =
            slotGet slots (id + dir)
        dir : Int
        dir =
            if (moreHead == Empty)
            then 1
            else (-1)
        moreHead : View
        moreHead =
            withDefault Empty (List.head slots.more)
        moreTail : List (View)
        moreTail =
            withDefault [] (List.tail slots.more)
    in
    { slots
        | main = Array.indexedMap nextResult slots.main
        , more = moreTail
    }

slotRemoveMore : Slots -> Int -> Slots
slotRemoveMore slots id =
    let oldMore = slots.more
    in
    { slots
        | more = (List.take id oldMore) ++ (List.drop (id + 1) oldMore)
    }

slotMove2EndFromMore : Slots -> Int -> Slots
slotMove2EndFromMore slots id =
    let newMore : List View
        newMore =
            ((List.take id slots.more) ++ (List.drop (id + 1) slots.more))
        moreSlot : View
        moreSlot =
            withDefault Empty (List.head (List.drop id slots.more))
        newSlots : Slots
        newSlots =
            slotChangeTo
                (slotChangeTo
                    (slotChangeTo
                        { slots | more = ((slotGet slots 0)::newMore)}
                        0 (slotGet slots 1))
                    1 (slotGet slots 2))
                2 moreSlot
    in
    newSlots

primaryColor : Property c m
--primaryColor = Color.background Color.primaryContrast
primaryColor = cs "primaryColor"

generalBackgroundColor : Property c m
generalBackgroundColor = cs "generalBackgroundColor"
