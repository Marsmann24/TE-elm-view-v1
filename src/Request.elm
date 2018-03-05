module Request exposing (..)

import Model exposing (Msg(..))
--, Command(..))
import Topic
import Term
import Document

import ContainerCache
import Http
import Json.Decode exposing (Decoder)

baseURL : String
baseURL = "https://topicexplorer.informatik.uni-halle.de/09sdfjglikqw3bret5cp84vqyolrfiksefgdakyuheas/webapp/ZEIT0614_3_te/JsonServlet?Command="

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

loadDocTokens : Int -> Cmd Msg
loadDocTokens id =
    loadData Document.documentDecoder (NewDocTokens) ("getDoc&DocId=" ++ (toString id))

loadBestDocs : Int -> Int -> String -> Cmd Msg
loadBestDocs id term sorting =
    let command = String.concat ["bestDocs&TopicId=", (toString id), termArgument, "&sorting=", sorting]
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

loadAutocompleteTerms : String -> Cmd Msg
loadAutocompleteTerms termName =
    let command = "autocomplete&SearchWord=" ++ termName
    in
    loadData Term.searchTermDecoder (NewTermTopics termName) command

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
--createGetTopics : (ContainerCache.Meta TopicResult -> Int -> ContainerCache.Page TopicResult)
--createGetTopics =
--     "getTopics" Topic.decodeTopics

--createGetDoc : Int -> (ContainerCache.Meta TopicResult -> Int -> ContainerCache.Page TopicResult)
--createGetDoc docId =
--    createRequest ("getDoc&DocId=" ++ (toString id)) Document.documentDecoder

--createGetBestDocs : Int -> Int -> String -> (ContainerCache.Meta TopicResult -> Int -> ContainerCache.Page TopicResult)
--createGetBestDocs topicId term sorting =
--    let command = String.concat ["getBestDocs&TopicId=", (toString id), termArgument, "&sorting=", sorting]
--        termArgument =
--            if (term >= 0)git:
--            then ("&term" ++ (toString term))
--            else ""
--    in
--    createRequest command Document.bestDocsDecoder

--createGetTermRequest : Int -> (ContainerCache.Meta TermsResult -> Int -> ContainerCache.Page TermsResult)
--createGetTermRequest topicId =
--    createRequest ("getTerms&TopicId=" ++ (toString topicId)) Term.termsDecoder

--createGetBestTerms : (ContainerCache.Meta (List Term) -> Int -> ContainerCache.Page (List Term))
--createGetBestTerms =
--    createRequest "getBestTerms" Term.bestTermsDecoder

--createGetBestFrames : (ContainerCache.Meta (List Term) -> Int -> ContainerCache.Page (List Term))
--createGetBestFrames =
--    createRequest "getBestFrames" Term.bestTermsDecoder

--createRequest : String -> Decoder a -> ContainerCache.Meta a -> Int -> ContainerCache.Page a
--createRequest arguments decoder meta offset =
--    let url = baseURL ++ arguments
--        request = Http.get url decoder
--    in
--    ContainerCache.ToLoad meta.identifier
--        (Http.send (ContainerCache.LoadCheckPage meta offset)
--            (Http.get url meta.decoder)
--        )
