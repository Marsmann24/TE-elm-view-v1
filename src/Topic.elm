module Topic exposing (..)

import Term exposing (..)

import Json.Decode exposing (Decoder, string, list, map, map4, field, keyValuePairs)
import Decoderhelper exposing (int, pseudolist)

type alias Topic =
    { id : Int
    , hirarchical_topic : TopicHirarchie
    , color_topic : String
    , top_terms : TermSorting
    }

type alias TopicHirarchie =
    { start : Int
    , end : Int
    , depth : Int
    , cluster : String
    }

type alias TopicResult =
    { topics : List Topic
    , sorting : List Int
    , terms : List Term
    , topicsBestItemLimit : Int
    }

-- Mapper and Checker
termInTopic : Term -> Topic -> Bool
termInTopic term topic =
    List.member term.id (List.map (\x -> x.id) topic.top_terms)

topicId2Topic : List Topic -> Int -> Maybe Topic
topicId2Topic topics topicId =
    (List.head (List.filter (\x -> x.id == topicId) topics))

-- Decoders
topicDecoder : Decoder Topic
topicDecoder =
    map4 Topic
        (field "topic_id" int)
        (map4 TopicHirarchie
            (field "hirarchical_topic$start" int)
            (field "hirarchical_topic$end" int)
            (field "hirarchical_topic$depth" int)
            (field "hirarchical_topic$cluster" string)
        )
        (field "color_topic$color" string)
        (field "top_terms" termSortingDecoder)

decodeTopics : Decoder TopicResult
decodeTopics =
    map4 TopicResult
        (field "Topic" (pseudolist topicDecoder))
        (field "TOPIC_SORTING" (list int))
        (field "Term" (pseudolist termDecoder))
        (field "TopicBestItemLimit" int)
