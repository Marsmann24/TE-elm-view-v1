module Model exposing (..)

import Term exposing (..)
import Topic exposing (..)
import Document exposing (..)
import Slots exposing (Slots)

import ContainerCache
import Material
import Material.Options exposing (Property, cs, css)
import Material.Color as Color
import Material.Icon as Icon
import Http
import Html exposing (Html)
import Array exposing (Array)
import Maybe exposing (Maybe, withDefault)

type Msg
    = ResetSettings
    | Mobile Bool
    | Search String
    | AdvancedSearch String
    | Found View
    | SelectItem (Int, Int)
    | SelectTab Int
    | CloseTab
    | Raise Int
    | ChangeCurrentDoc Int Doc
    | RemoveTopic Int
    | DeleteSlot Int
    | RemoveSlot Int
    | MoveLeft
    | MoveRight
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
    | Toggle Settings
    | SetParent Parent
    --| ToggleBottom
    --| ToggleView2
    --| ToggleShowSaved
    | NewTopics Parent String Int (Result Http.Error (List Topic))
    | NewDocument Parent (Result Http.Error Document)
    | NewDocs Parent String Int (Result Http.Error (List Doc))
    | NewDocTokens Parent String Int (Result Http.Error Document)
    | NewTerms Parent String Int (Result Http.Error (List Term))
    | NewFrames Parent String Int (Result Http.Error (List Term))
    | NewTermTopics Parent String Int (Result Http.Error (List Term))
    | NewSearchTopics Parent String (Result Http.Error (List Term))
    | NewSearchTerms Parent String (Result Http.Error (List Term))
    | NewSearchDocs Parent String (Result Http.Error (List Doc))
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
    , slots : Slots View
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
    , mobile : Bool
    , docview : Bool
    , view2 : Bool
    , parent : Parent
    , showRelevance : Bool
    , showSlotDialoge : Bool
    , search : Bool
    , search4 : String
    , searchResult : SearchResult
    , slotToDelete : Int
    , selectedItem : (Int, Int)
    }

type Tab
    = PreviewTab
    | DocumentTab String Document
    | ErrorTab String String

type SearchResult
    = TopicResult (List Topic)
    | TermResult (List Term)
    | DocumentResult Doc

--Slots

type Parent
    = Termparent Int
    | Docparent Int
    | Topicparent Int
    | Noparent

getActive : Parent -> Settings -> Bool
getActive parent settings =
    if ((parent == settings.parent) || (settings.parent == Noparent))
        then True
        else False

isParent : Int -> Settings -> Bool
isParent id settings =
    case settings.parent of
        Termparent par ->
            if (par == id)
                then True
                else False
        Docparent par ->
            if (par == id)
                then True
                else False
        Topicparent par ->
            if (par == id)
                then True
                else False
        _ ->
            False

type View
    = TermsView String (List Term) Parent
    -- WordlistView (List String)
    | TopicsView String (List Topic) Int Parent
    --| TopicsView (ContainerCache.Container (List Topic))
    --| DocumentsView (List Document)
    --| DocumentsView (ContainerCache.Container (List Doc))
    | DocumentsView String (List Doc) Parent
    | Dialog
    | Empty
    | ErrorSlot


--type alias Slots =
--    { main : Array View
--    , more : List View
--    }

--slotFromTo : Slots -> View -> View -> Slots
--slotFromTo slots from to =
--    let findSlot = slotGetFirstId slots from
--    in
--    if (findSlot == -1)
--    then
--        slotChangeTo
--            (slotChangeTo
--                (slotChangeTo
--                    { slots | more = ((slotGet slots 0)::slots.more)}
--                    0 (slotGet slots 1))
--                1 (slotGet slots 2))
--            2 to
--    else slotChangeTo slots findSlot to
--
--slotsCount : Slots -> Int
--slotsCount slots =
--    let firstEmpty =
--            slotGetFirstId slots Empty
--    in
--    if (firstEmpty < 0)
--    then Array.length slots.main
--    else firstEmpty
--
--slotGetFirstId : Slots -> View -> Int
--slotGetFirstId slots view =
--    slotGetFirstIdSince slots view 0
--
--slotGetFirstIdSince : Slots -> View -> Int -> Int
--slotGetFirstIdSince slots view id =
--    let getView =
--        case (Array.get id slots.main) of
--            Just a ->
--                a
--            Nothing ->
--                ErrorSlot
--    in
--    if (getView == ErrorSlot)
--    then -1
--    else if (getView == view)
--        then id
--        else (slotGetFirstIdSince slots view (id + 1))
--
--slotGet : Slots -> Int -> View
--slotGet slots slotId =
--    withDefault ErrorSlot (Array.get slotId slots.main)

--slotChangeTo : Slots -> Int -> View -> Slots
--slotChangeTo oldSlots id value =
--    { oldSlots | main = Array.set id value oldSlots.main}

--slotRemove : Slots -> Int -> Slots
--slotRemove slots removeId =
--    let
--        nextResult : Int -> View -> View
--        nextResult id view =
--            if ((nextSlot id) == ErrorSlot)
--            then moreHead
--            else if ((id * dir) >= (removeId * dir))
--                then nextSlot id
--                else view
--        nextSlot : Int -> View
--        nextSlot id =
--            slotGet slots (id + dir)
--        dir : Int
--        dir =
--            if (moreHead == Empty)
--            then 1
--            else (-1)
--        moreHead : View
--        moreHead =
--            withDefault Empty (List.head slots.more)
--        moreTail : List (View)
--        moreTail =
--            withDefault [] (List.tail slots.more)
--    in
--    { slots
--        | main = Array.indexedMap nextResult slots.main
--        , more = moreTail
--    }
--
--slotRemoveMore : Slots -> Int -> Slots
--slotRemoveMore slots id =
--    let oldMore = slots.more
--    in
--    { slots
--        | more = (List.take id oldMore) ++ (List.drop (id + 1) oldMore)
--    }
--
--slotMove2EndFromMore : Slots -> Int -> Slots
--slotMove2EndFromMore slots id =
--    let newMore : List View
--        newMore =
--            ((List.take id slots.more) ++ (List.drop (id + 1) slots.more))
--        moreSlot : View
--        moreSlot =
--            withDefault Empty (List.head (List.drop id slots.more))
--        newSlots : Slots
--        newSlots =
--            slotChangeTo
--                (slotChangeTo
--                    (slotChangeTo
--                        { slots | more = ((slotGet slots 0)::newMore)}
--                        0 (slotGet slots 1))
--                    1 (slotGet slots 2))
--                2 moreSlot
--    in
--    newSlots

iconHighlighted : Settings -> (Int, Int) -> List (Icon.Property m)
iconHighlighted settings id =
    if (id == settings.selectedItem)
    then
        [ Icon.size48
        , Color.text Color.primary
        , Color.background Color.primaryContrast
        , css "border-radius" "10px"
        , css "margin" "10px"
        , css "padding" "10px"
        ]
    else
        []

primaryColor : Property c m
--primaryColor = Color.background Color.primaryContrast
primaryColor = cs "primaryColor"

generalBackgroundColor : Property c m
generalBackgroundColor = cs "generalBackgroundColor"
