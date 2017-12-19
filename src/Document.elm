module Document exposing (..)

import Topic exposing (pseudolist)

import Json.Decode exposing (Decoder, string, int, list, map4, map8, field, keyValuePairs)

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

type alias BestDocs =
    (List
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
    , List Int
    )

--decoder
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

bestDocsDecoder : Decoder BestDocs
bestDocsDecoder =
    let documents =
            field "DOCUMENT"
                (pseudolist
                    (map10
                        (field "DOCUMENT_ID" int)
                        (field "TOPIC_ID" int)
                        (field "DOCUMENT_COUNT" int)
                        (field "KEYWORD_SNIPPET" string)
                        (field "KEYWORD_TITLE" string)
                        (field "TOP_TOPIC" (list int))
                        (field "LINK$URL" string)
                        (field "TEXT$TIME_STAMP" int)
                        (field "TEXT$TITLE" string)
                        (field "TEXT$SNIPPET" string)
                    )
                )
        sorting =
            field "DOCUMENT_SORTING" (list int)
    in
    (documents, sorting)
