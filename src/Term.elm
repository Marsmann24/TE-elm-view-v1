module Term exposing (..)

import Json.Decode exposing (Decoder, string, int, list, map, map2, map5, field, keyValuePairs, succeed, maybe)
import Decoderhelper exposing (int2, intDictDecoder, listheadwithdefault, pseudolist)
import Dict exposing (Dict)

type alias Term =
    { id : Int
    , name : String
    , wordtype : Maybe Int
    , count : Maybe Int
    , top_topic : List Int
    }

type alias TermSorting =
    List { id : Int, relevance : Int}

type alias TermsResult =
    { topic : (Int, TermSorting)
    , terms : Dict Int Term
    }

-- Mapper and Checker
defaultTerm : Term
defaultTerm =
    (Term -1 "Error: Not matching." Nothing Nothing [])

termId2Term : List Term -> Int -> Maybe Term
termId2Term terms termId =
    (List.head (List.filter (\x -> x.id == termId) terms))

matchTermsById : { items : Dict Int Term, sorting : List Int} -> List Term
matchTermsById termsorting =
    List.map
        (\x -> Maybe.withDefault defaultTerm (Dict.get x termsorting.items))
        termsorting.sorting

matchTermsortingById : TermsResult -> List Term
matchTermsortingById termsresult =
    let termsorting : { items : Dict Int Term, sorting : List Int}
        termsorting =
            { items = termsresult.terms
            , sorting = List.map .id (List.sortBy .relevance (Tuple.second termsresult.topic))
            }
    in
    matchTermsById termsorting

-- Decoders
termDecoder : Decoder Int -> Decoder Term
termDecoder intDecoder =
    map5 Term
        (field "TERM_ID" intDecoder)
        (field "TERM_NAME" string)
        (maybe (field "WORDTYPE$WORDTYPE" intDecoder))
        (succeed Nothing)
        (map
            (Maybe.withDefault [])
            (maybe (field "TOP_TOPIC" (list int)))
        )

termSortingDecoder : Decoder TermSorting
termSortingDecoder =
    list
        (map2 (\x y -> {id = x, relevance = y})
            (field "TermId" int2)
            (field "relevance" int2)
        )

termsDecoder : Decoder (List Term)
termsDecoder =
    map matchTermsortingById
        (map2 TermsResult
            (field "Topic"
                (map
                    (Tuple.mapFirst (\x -> Result.withDefault -1 (String.toInt x)))
                    (listheadwithdefault
                        ("0", [{id = 0, relevance = 0}])
                        (keyValuePairs (field "Top_Terms" termSortingDecoder))
                    )
                )
            )
            (field "Term" (intDictDecoder defaultTerm (termDecoder int)))
        )

bestTermsDecoder : Decoder (List Term)
bestTermsDecoder =
    --pseudolist
    map matchTermsById
        (field "61"
            (listheadwithdefault { sorting = [], items = Dict.empty}
                (field "ITEMS"
                    (pseudolist
                        (map2 (\x y->{ sorting = x, items = y})
                            (field "SORTING" (list int))
                            (intDictDecoder
                                defaultTerm
                                (map5 Term
                                        (field "ITEM_ID" int)
                                        (field "ITEM_NAME" string)
                                        (succeed Nothing)
                                        (maybe (field "ITEM_COUNT" int))
                                        (succeed [])
                                )
                            )
                        )
                    )
                )
            )
        )


searchTermDecoder : Decoder (List Term)
searchTermDecoder =
    list (termDecoder int2)
