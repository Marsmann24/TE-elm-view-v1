module Request exposing (..)

import Model exposing (Msg(..))
--, Command(..))
import Topic exposing (Topic)
import Term exposing (Term)
import Document exposing (Doc, Document)

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
    loadData Topic.decodeTopics (NewTopics "Topics") "getTopics"

loadDoc : Doc -> Cmd Msg
loadDoc doc =
    loadData Document.documentDecoder (NewDocument) ("getDoc&DocId=" ++ (toString doc.document_id))

loadDocTokens : Doc -> Cmd Msg
loadDocTokens doc =
    loadData Document.documentDecoder (NewDocTokens (("Terms in \"" ++ doc.title) ++ "\"")) ("getDoc&DocId=" ++ (toString doc.document_id))

loadBestDocs : Topic -> Maybe Term -> String -> Cmd Msg
loadBestDocs topic maybeterm sorting =
    let command = String.concat ["bestDocs&TopicId=", (toString topic.id), termArgument, "&sorting=", sorting]
        termArgument =
            case maybeterm of
                Just term ->
                    "&term" ++ (toString term)
                _ ->
                    ""
        name =
            case maybeterm of
                Just term ->
                    "Docs with " ++ term.name
                _ ->
                    "Docs in Topic " ++ (toString topic.id)
    in
    loadData Document.bestDocsDecoder (NewDocs name) command

loadTerms : Topic -> Int -> Cmd Msg
loadTerms topic offset =
    let command = String.concat ["getTerms&TopicId=", (toString topic.id), "&offset=",(toString offset)]
    in
    loadData Term.termsDecoder (NewTerms ("Terms in Topic " ++ (toString topic.id))) command

loadBestTerms : Cmd Msg
loadBestTerms =
    loadData Term.bestTermsDecoder (NewTerms "Terms") "getBestTerms"

loadAutocompleteTerms : String -> Cmd Msg
loadAutocompleteTerms termName =
    let command = "autocomplete&SearchWord=" ++ termName
    in
    loadData Term.searchTermDecoder (NewTermTopics termName) command

loadBestFrames : Cmd Msg
loadBestFrames =
    loadData Term.bestTermsDecoder (NewFrames "Frames") "getBestFrames"

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
