module Decoderhelper exposing (..)

import Json.Decode exposing (Decoder, map, map2, map5, keyValuePairs, string)
import Dict exposing (Dict)
import Result exposing (Result(..))

stringAsInt : Decoder Int
stringAsInt =
    let convert : String -> Int
        convert a = Result.withDefault 0 (String.toInt a)
    in
    map convert string

pseudolist : Decoder a -> Decoder (List a)
pseudolist decoder =
    map (List.map Tuple.second) (keyValuePairs decoder)

listheadwithdefault : a -> Decoder (List a) -> Decoder a
listheadwithdefault  default decoder =
    map (\x -> Maybe.withDefault default (List.head x)) decoder

intDictDecoder : a -> Decoder a -> Decoder (Dict Int a)
intDictDecoder default decoder =
    let makeInt : (String, a) -> (Int, a)
        makeInt (key, value) =
            case (String.toInt key) of
                Ok result ->
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

map10 : (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> b) -> Decoder a0 -> Decoder a1 -> Decoder a2 -> Decoder a3 -> Decoder a4 -> Decoder a5 -> Decoder a6 -> Decoder a7 -> Decoder a8 -> Decoder a9 -> Decoder b
map10 function decoder0 decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9 =
    (map2
        (\a b -> function a.a1 a.a2 a.a3 a.a4 a.a5 b.b1 b.b2 b.b3 b.b4 b.b5)
        (map5
            (\a b c d e ->
                { a1 = a
                , a2 = b
                , a3 = c
                , a4 = d
                , a5 = e
                }
            )
            decoder0 decoder1 decoder2 decoder3 decoder4
        )
        (map5
            (\a b c d e ->
                { b1 = a
                , b2 = b
                , b3 = c
                , b4 = d
                , b5 = e
                }
            )
            decoder5 decoder6 decoder7 decoder8 decoder9
        )
    )
