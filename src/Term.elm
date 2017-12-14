module Term exposing (..)

import Json.Decode exposing (Decoder, string, int, list, map, map2, map3, field, keyValuePairs)
import Dict
import Tuple

type alias Term =
    { id : Int
    , name : String
    , wordtype : Maybe Int
    }

type alias TermSorting =
    List { id : Int, relevance : Int}

type alias GetTermsResult =
    { topic : (Int, TermSorting)
    , terms : Dict Int Term
    }

-- decoder
intDictDecoder : Decoder a -> Decoder (Dict Int a)
intDictDecoder decoder =
    map
        Dict.fromList
        (map
            (List.map (Tuple.mapFirst toInt))
            (keyValuePairs decoder)
        )

termDecoder : Decoder Term
termDecoder =
    map3 Term
        (field "TERM_ID" int)
        (field "TERM_NAME" string)
        (maybe (field "WORDTYPE$WORDTYPE" int))

termSortingDecoder : Decoder TermSorting
termSortingDecoder =
    list
        (map2 TermSorting
            (field "TermId" int)
            (field "relevance" int)
        )

getTermsDecoder : Decoder GetTermsResult
getTermsDecoder =
    map2 GetTermsResult
        (field "Topic"
            (map
                (Tuple.mapFirst toInt)
                (map
                    (Maybe.withDefault (0, {id = 0, relevance = 0}) List.head)
                    (keyValuePairs (field "Top_Terms" termSortingDecoder))
                )
            )
        )
        (field "Term" (intDictDecoder termDecoder))
