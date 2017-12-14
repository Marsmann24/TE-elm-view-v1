module Term exposing (..)

type alias Term =
    { id : Int
    , name : String
    , wordtype : Maybe Int
    }

type alias TermSorting =
    List { id : Int, relevance : Int}

type alias GetTermsResult =
    { topic : [(String, TermSorting)]
    , terms : Dict String Term
    }

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
        (field "Topic" (keyValuePairs (field "Top_Terms" termSortingDecoder)))
        (field "Term" (dict termDecoder))
