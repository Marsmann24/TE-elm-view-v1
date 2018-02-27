module Topic exposing (..)

import Term exposing (..)

import Json.Decode exposing (Decoder, string, int, list, map, map3, map4, field, keyValuePairs)
import Decoderhelper exposing (intDictDecoder)
import Dict exposing (Dict)

type alias RawTopic =
    { id : Int
    , hirarchical_topic : TopicHirarchie
    , color_topic : String
    , top_terms : TermSorting
    }

type alias Topic =
    { id : Int
    , hirarchical_topic : TopicHirarchie
    , color_topic : String
    , top_terms : List Term
    }

type alias TopicHirarchie =
    { start : Int
    , end : Int
    , depth : Int
    , cluster : String
    }

--type alias TopicResult =
--    { topics : List Topic
--    , sorting : List Int
--    , terms : List Term
--    , topicsBestItemLimit : Int
--    }

-- Mapper and Checker
termInTopic : Term -> Topic -> Bool
termInTopic term topic =
    List.member term.id (List.map (\x -> x.id) topic.top_terms)

topicId2Topic : List Topic -> Int -> Maybe Topic
topicId2Topic topics topicId =
    (List.head (List.filter (\x -> x.id == topicId) topics))

matchRawTopicsById : Dict Int RawTopic -> List Int -> List RawTopic
matchRawTopicsById topics sorting =
    List.map
        (\x -> Maybe.withDefault defaultRawTopic (Dict.get x topics))
        sorting

makeTopicsList : Dict Int RawTopic -> List Int -> Dict Int Term -> List Topic
makeTopicsList topics sorting terms =
    let matchTerms : Int -> TermSorting -> List Term
        matchTerms topic topic_terms =
            matchTermsortingById { topic = (topic, topic_terms) , terms = terms}
        rawTopic2Topic : RawTopic -> Topic
        rawTopic2Topic raw =
            Topic raw.id raw.hirarchical_topic raw.color_topic (matchTerms raw.id raw.top_terms)
    in
    List.map rawTopic2Topic (matchRawTopicsById topics sorting)

defaultRawTopic : RawTopic
defaultRawTopic =
    (RawTopic -1 (TopicHirarchie -1 -1 -1 "") "Error: not matching" [])

-- Decoders
topicDecoder : Decoder RawTopic
topicDecoder =
    map4 RawTopic
        (field "topic_id" int)
        (map4 TopicHirarchie
            (field "hirarchical_topic$start" int)
            (field "hirarchical_topic$end" int)
            (field "hirarchical_topic$depth" int)
            (field "hirarchical_topic$cluster" string)
        )
        (field "color_topic$color" string)
        (field "top_terms" termSortingDecoder)

decodeTopics : Decoder (List Topic)
decodeTopics =
    map3 makeTopicsList
        (field "Topic" (intDictDecoder defaultRawTopic topicDecoder))
        (field "TOPIC_SORTING" (list int))
        (field "Term" (intDictDecoder defaultTerm termDecoder))
        --(field "TopicBestItemLimit" int)
