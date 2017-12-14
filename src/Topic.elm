module Topic exposing (..)

import Term

import Json.Decode exposing (Decoder, string, int, list, map3, field, succeed)
import Json.Decode.Helper exposing (pseudolist, decodeValue)

type alias Topic =
    { id : Int
    , hirarchical_topic :
        { start : Int
        , end : Int
        , depth : Int
        , cluster : String
        }
    , color_topic : String
    , top_terms : TermSorting
    }

-- Setter
setTopicId : Topic -> Int -> Topic
setTopicId topic value = { topic | id = value}

setHirarchicalTopicStart : Topic -> Int -> Topic
setHirarchicalTopicStart topic value =
    let new_hirarchical_topic = { topic.hirarchical_topic | start = value}
    in
    { topic | hirarchical_topic = new_hirarchical_topic}

setHirarchicalTopicEnd : Topic -> Int -> Topic
setHirarchicalTopicEnd topic value =
    let new_hirarchical_topic = { topic.hirarchical_topic | end= value}
    in
    { topic | hirarchical_topic = new_hirarchical_topic}

setHirarchicalTopicDepth : Topic -> Int -> Topic
setHirarchicalTopicDepth topic value =
    let new_hirarchical_topic = { topic.hirarchical_topic | depth = value}
    in
    { topic | hirarchical_topic = new_hirarchical_topic}

setHirarchicalTopicCluster : Topic -> String -> Topic
setHirarchicalTopicCluster topic value =
    let new_hirarchical_topic = { topic.hirarchical_topic | cluster = value}
    in
    { topic | hirarchical_topic = new_hirarchical_topic}

setColorTopicColor : Topic -> String -> Topic
setTopicId topic value = { topic | color_topic = value}

setTopicTopTerms : Topic -> TermSorting -> Topic
setTopicTopTerms topic value = { topic | top_terms = value}

-- raw topics data
type alias Topics =
    { topics : List Topic
    , sorting : List Int
    , terms : List Term
    , topicsBestItemLimit : Int
    }

setTopics : Topics -> List Topic -> Topcis
setTopics topicsa value = { topicsa | topics = value}

setSorting : Topics -> List Int -> Topcis
setSorting topicsa value = { topicsa | sorting = value}

setTopics : Topics -> List Term -> Topcis
setTopics topicsa value = { topics | terms = value}

setTopicsBestItem : Topics -> Int -> Topcis
setTopicsBestItem topicsa value = { topicsa | topicsBestItemLimit = value}

-- decoder
topicDecoder : Decoder Topic
topicDecoder =
    let
        convert : Value -> Result Topic
        convert value =
            succeed
                { id = 0
                , hirarchical_topic =
                    { start = 0
                    , end = 0
                    , depth = 0
                    , cluster = ""
                    }
                , color_topic : { color = ""}
                , top_terms : [{ id = 0, relevance = 0}]
                }
                |> decodeField "topic_id" int setTopicId value
                |> decodeField "hirarchical_topic$start" int setHirarchicalTopicStart value
                |> decodeField "hirarchical_topic$end" int setHirarchicalTopicEnd value
                |> decodeField "hirarchical_topic$depth" int setHirarchicalTopicDepth value
                |> decodeField "hirarchical_topic$cluster" string setHirarchicalTopicCluster value
                |> decodeField "color_topic$color" string setColorTopicColor value
                |> decodeField "top_terms"
                    (list
                        (map2 TermSorting
                            (field "TermId" int)
                            (field "relevance" int)
                    ))
                    setTopicTopTerms value
    in
        string |> andThen convert

decodeTopics : Decoder Topic
decodeTopics =
    let convert : Value -> Result Topic
        convert value =
            succeed
                { topic : List Topic
                , topic_sorting : List Int
                , terms : List Term
                , topicBestItemLimit : Int
                }
                |> decodeField "Topic" (pseudolist decodeTopic) setTopics value
                |> decodeField "TOPIC_SORTING" (list int) setSorting value
                |> decodeField "Term" (pseudolist termDecoder) setTerms value
                |> decodeField "TopicBestItemLimit" int setTopicsBestItem value
    in
        string |> andThen convert
