module Json.Decode.Helper exposing (pseudolist, decodeField)

import Json.Decode exposing (Decoder, string, field, keyValuePairs, succeed)

decodeField : String -> Decoder a -> (b -> a -> b) -> Value -> Result String b -> Result String b
decodeField fieldName decoder setter value b =
    let
        decoded =
            decodeValue (field fieldName decoder) value
    in
    case b of
        Err s ->
            case decoded of
                Err newMessage ->
                    Err (s ++ "\nAnd " ++ newMessage)
                Ok _ ->
                    b
         Ok v ->
            Result.map (setter v) decoded

pseudolist : Decoder a -> Decoder (List a)
pseudolist decoder =
    let convert value =
            suceed
                (List.map
                    Tupple.second
                    (decodeString (keyValuePairs decoder))
                )
    in
        string |> andThen convert
