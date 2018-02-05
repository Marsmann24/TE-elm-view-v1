module Request exposing (..)

import Model exposing (Msg(..))
--, Command(..))
import Topic
import Term
import Document

--import DB
--import ManageDBs
import Http
import Json.Decode exposing (Decoder)

baseURL : String
baseURL = "http://topicexplorer.informatik.uni-halle.de/09sdfjglikqw3bret5cp84vqyolrfiksefgdakyuheas/webapp/ZEIT0614_3_te/JsonServlet?Command="

loadData : Decoder a -> (Result Http.Error a -> Msg) -> String -> Cmd Msg
loadData decoder msg arguments =
    let url = (baseURL ++ arguments)
        request = Http.get url decoder
    in
    Http.send msg request

loadTopics : Cmd Msg
loadTopics =
    loadData Topic.decodeTopics (NewTopics) "getTopics"

loadDoc : Int -> Cmd Msg
loadDoc id =
    loadData Document.documentDecoder (NewDocument) ("getDoc&DocId=" ++ (toString id))

loadBestDocs : Int -> Int -> String -> Cmd Msg
loadBestDocs id term sorting =
    let command = String.concat ["getBestDocs&TopicId=", (toString id), termArgument, "&sorting=", sorting]
        termArgument =
            if (term >= 0)
            then ("&term" ++ (toString term))
            else ""
    in
    loadData Document.bestDocsDecoder (NewDocs) command

loadTerms : Int -> Int -> Cmd Msg
loadTerms id offset =
    let command = String.concat ["getTerms&TopicId=", (toString id), "&offset=",(toString offset)]
    in
    loadData Term.termsDecoder (NewTerms) command

loadBestTerms : Cmd Msg
loadBestTerms =
    loadData Term.bestTermsDecoder (NewTerms) "getBestTerms"

loadBestFrames : Cmd Msg
loadBestFrames =
    loadData Term.bestTermsDecoder (NewFrames) "getBestFrames"

-- Command Struktur
--loadData : Command -> String -> Cmd Msg
--loadData command arguments =
--    let url = (baseURL ++ (toString command)) ++ arguments
--        request = Http.get url (getDecoder command)
--    in
--    Http.send (getMsg command) request

--getDecoder : Command -> Decoder a
--getDecoder command =
--    case command of
--        GetTopics ->
--            Topic.decodeTopics
--        GetDoc ->
--            Document.documentDecoder
--        GetBestDocs ->
--            Document.bestDocsDecoder
--        GetTerms ->
--            Term.termsDecoder
--        GetBestTerms ->
--            Term.bestTermsDecoder
--        GetBestFrames ->
--            Term.bestTermsDecoder
--
--getMsg : Command -> a -> Msg
--getMsg command a = None
    --case command of
    --    GetTopics ->
    --        NewTopics a
    --    GetDoc ->
    --        NewDocs a
    --    GetBestDocs ->
    --        NewDocs a
    --    GetTerms ->
    --        NewTerms a
    --    GetBestTerms ->
    --        NewTerms a
    --    GetBestFrames ->
    --        NewFrames a

-- Chris Einbindung
--createGetTermRequest : DB.DBMeta TermsResult -> Int -> String -> DB.Page TermsResult
--createGetTermRequest meta offset arguments=
--    createRequest meta GetTerms arguments offset
--
--createRequest : DB.DBMeta a -> Command -> String -> Int -> DB.Page a
--createRequest meta command arguments offset =
--    let url = (baseURL ++ (toString command)) ++ arguments
--        request = Http.get url (getDecoder command)
--    in
--    DB.ToLoad meta.identifier
--        (Http.send (DB.LoadCheckPage meta offset)
--            (Http.get url meta.decoder)
--        )
