module Document exposing (..)

import Topic exposing (..)
import Term exposing (..)

import Json.Decode exposing (Decoder, int, string, list, map2, map4, map8, field, keyValuePairs)
import Decoderhelper exposing (int2, map10, pseudolist, listheadwithdefault)

-- Types
type alias Document =
    { id : Int
    , linkurl : String
    , time_stamp : Int
    , title : String
    , fulltext : String
    , search_test : String
    , frame_list :  List String
    , word_list : List Token
    }

type alias Token =
    { topic_id : Int
    , posintion_in_document : Int
    , term : String
    , parent_topic_ids : List Int
    }

type alias Doc =
    { document_id : Int
    , topic_id : Int
    , document_count : String
    , keyword_snippet : String
    , keyword_title : String
    , top_topic : List Int
    , linkurl : String
    , time_stamp : Int
    , title : String
    , snippet : String
    }

-- Mapper and Checker
docInTopic : Doc -> Topic -> Bool
docInTopic doc topic =
    if (doc.topic_id == topic.id)
    then True
    else List.member topic.id doc.top_topic

termInDocument : Term -> Document -> Bool
termInDocument term document =
    List.member term.name (List.map .term document.word_list)

topicInDoc : Topic -> Doc -> Bool
topicInDoc topic doc =
    if (topic.id == doc.topic_id)
        then True
        else False

documentId2Document : List Document -> Int -> Maybe Document
documentId2Document documents documentId =
    (List.head (List.filter (\x -> x.id == documentId) documents))

docId2Doc : List Doc -> Int -> Maybe Doc
docId2Doc docs docId =
    (List.head (List.filter (\x -> x.document_id == docId) docs))

-- Decoders :
tokenDecoder : Decoder Token
tokenDecoder =
    map4 Token
        (field "TOPIC_ID" int)
        (field "POSITION_OF_TOKEN_IN_DOCUMENT" int)
        (field "TOKEN" string)
        (field "HIRARCHICAL_TOPIC$PARENT_IDS" (list int))

documentDecoder : Decoder Document
documentDecoder =
    field "DOCUMENT"
        (map8 Document
            (field "DOCUMENT_ID" int)
            (field "LINK$URL" string)
            (field "TIME$TIME_STAMP" int)
            (field "TEXT$TITLE" string)
            (field "TEXT$FULLTEXT" string)
            (field "SEARCH_TEXT" string)
            (field "FRAME_LIST" (listheadwithdefault [] (pseudolist (list string))))
            (field "WORD_LIST" (list tokenDecoder))
        )

bestDocsDecoder : Decoder (List Doc)
bestDocsDecoder =
    let -- Decoders :
        documents : Decoder (List Doc)
        documents =
            field "DOCUMENT"
                (pseudolist
                    (map10 Doc
                        (field "DOCUMENT_ID" int2)
                        (field "TOPIC_ID" int2)
                        (field "PR_DOCUMENT_GIVEN_TOPIC" string)
                        (field "KEYWORD_SNIPPET" string)
                        (field "KEYWORD_TITLE" string)
                        (field "TOP_TOPIC" (list int))
                        (field "LINK$URL" string)
                        (field "TIME$TIME_STAMP" int)
                        (field "TEXT$TITLE" string)
                        (field "TEXT$SNIPPET" string)
                    )
                )

        sorting : Decoder (List Int)
        sorting =
            field "DOCUMENT_SORTING" (list int)

        -- Combinefunction :
        getDoc : List Doc -> Int -> Maybe Doc
        getDoc docs docId =
            List.head
                (List.filter
                    (\a -> a.document_id == docId)
                    docs
                )

        applySorting : List Doc -> List Int -> List Doc
        applySorting docs order =
            List.filterMap (getDoc docs) order
    in
    -- combine Decoders :
    map2 applySorting documents sorting
