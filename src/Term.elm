module Term exposing (..)

import Json.Decode exposing (Decoder, string, int, list, map, map2, map4, field, keyValuePairs, succeede)
import Dict
import Tuple

type alias Term =
    { id : Int
    , name : String
    , wordtype : Maybe Int
    , count : Maybe Int
    }

type alias TermSorting =
    List { id : Int, relevance : Int}

type alias TermsResult =
    { topic : (Int, TermSorting)
    , terms : Dict Int Term
    }

-- decoder
intDictDecoder : Decoder a -> a -> Decoder (Dict Int a)
intDictDecoder decoder default=
    let makeInt (key, value) =
            case (toInt key) of
                OK result ->
                    (result, value)
                _ ->
                    (-1, default)
    in
    map
        Dict.fromList
        (map
            (List.map makeInt)
            (keyValuePairs decoder)
        )

termDecoder : Decoder Term
termDecoder =
    map4 Term
        (field "TERM_ID" int)
        (field "TERM_NAME" string)
        (maybe (field "WORDTYPE$WORDTYPE" int))
        (succeede Nothing)

termSortingDecoder : Decoder TermSorting
termSortingDecoder =
    list
        (map2 TermSorting
            (field "TermId" int)
            (field "relevance" int)
        )

termsDecoder : Decoder TermsResult
termsDecoder =
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
        (field "Term" (intDictDecoder termDecoder (Term -1 "" Nothing Nothing))

bestTermsDecoder : Decoder
bestTermsDecoder =
    pseudolist
        (field "ITEMS"
            (keyValuePairs
                (map2 (/x y->{ sorting = x, items = y})
                    (field "SORTING" (list int))
                    (intDictDecoder
                        (map4 Term
                                (field "ITEM_ID" int)
                                (field "ITEM_NAME" string)
                                (succeede Nothing)
                                (maybe (field "ITEM_COUNT" int))
                        )
                        (Term -1 "" Nothing Nothing)
                    )
                )
            )
        )
