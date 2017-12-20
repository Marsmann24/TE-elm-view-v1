module Document exposing (..)

import Topic exposing (..)
import Term exposing (..)

import Json.Decode exposing (Decoder, string, int, list, map2, map4, map5, map8, field, keyValuePairs)

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
    , document_count : Int
    , keyword_snippet : String
    , keyword_title : String
    , top_topic : List Int
    , linkurl : String
    , time_stamp : Int
    , title : String
    , snipet : String
    }

-- Mapper and Checker
docInTopic : Doc -> Topic -> Bool
docInTopic doc topic =
    if (doc.topic_id == topic.id)
    then True
    else List.member topic.id doc.top_topic

termInDocument : Term -> Document -> Bool
termInDocument term document =
    List.member term.id (List.map .id document.word_list)

documentId2Document : List Document -> Int -> Maybe Document
documentId2Document documents documentId =
    (List.head (List.filter (\x -> x.document_id == documentId) documents))

documentId2Document : List Doc -> Int -> Maybe Doc
documentId2Document docs docId =
    (List.head (List.filter (\x -> x.id == docId) docs))

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
            (field "FRAME_LIST" (keyValuePairs (list string)))
            (field "WORD_LIST" (list tokenDecoder))
        )

bestDocsDecoder : Decoder (List Doc)
bestDocsDecoder =
    let -- Decoders :
        documents : Decoder (List Doc)
        documents =
            field "DOCUMENT"
                (pseudolist
                    (map2
                        (/ a b -> Doc a.a1 a.a2 a.a3 a.a4 a.a5 b.b1 b.b2 b.b3 b.b4 b.b5)
                        (map5
                            (/ a b c d e ->
                                { a1 = a
                                , a2 = b
                                , a3 = c
                                , a4 = d
                                , a5 = e
                                }
                            )
                            (field "DOCUMENT_ID" int)
                            (field "TOPIC_ID" int)
                            (field "DOCUMENT_COUNT" int)
                            (field "KEYWORD_SNIPPET" string)
                            (field "KEYWORD_TITLE" string)
                        )
                        (map5
                            (/ a b c d e ->
                                { b1 = a
                                , b2 = b
                                , b3 = c
                                , b4 = d
                                , b5 = e
                                }
                            )
                            (field "TOP_TOPIC" (list int))
                            (field "LINK$URL" string)
                            (field "TEXT$TIME_STAMP" int)
                            (field "TEXT$TITLE" string)
                            (field "TEXT$SNIPPET" string)
                        )
                    )
                )

        sorting : Decoder (List Int)
        sorting =
            field "DOCUMENT_SORTING" (list int)
            type alias Doc =
                { document_id : Int
                , topic_id : Int
                , document_count : Int
                , keyword_snippet : String
                , keyword_title : String
                , top_topic : List Int
                , linkurl : String
                , time_stamp : Int
                , title : String
                , snipet : String
                }

        -- Combinefunction :
        getDoc : List Doc -> Int -> Maybe Doc
        getDoc docs docId =
            List.head
                (List.filter
                    (/a -> a.document_id == docId)
                    docs
                )

        applySorting : List Doc -> List Int -> List Doc
        applySorting docs order =
            List.filterMap (getDoc docs) order
    in
    -- combine Decoders :
    map2 applySorting documents sorting
