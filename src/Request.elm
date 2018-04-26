module Request exposing (..)

import Model exposing (Msg(..), Parent(..))
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

loadTopics : Int -> Cmd Msg
loadTopics slotId =
    loadData Topic.decodeTopics (NewTopics (Noparent) "Topics" slotId) "getTopics"

loadDoc : Doc -> Cmd Msg
loadDoc doc =
    loadData Document.documentDecoder (NewDocument (Noparent)) ("getDoc&DocId=" ++ (toString doc.id))

loadDocTokens : Doc -> Int -> Cmd Msg
loadDocTokens doc slotId =
    loadData Document.documentDecoder (NewDocTokens (Docparent doc.id)(("Terms in \"" ++ doc.title) ++ "\"") slotId) ("getDoc&DocId=" ++ (toString doc.id))

loadBestDocs : Topic -> Maybe Term -> String -> Int -> Cmd Msg
loadBestDocs topic maybeterm sorting slotId =
    let command =
            String.concat
                [ "bestDocs&TopicId="
                , (toString topic.id)
                , termArgument
                , "&sorting="
                , sorting
                ]
        termArgument =
            case maybeterm of
                Just term ->
                    "&term" ++ (toString term)
                _ ->
                    ""
        parent =
            case maybeterm of
                Just term ->
                    Termparent term.id
                _ ->
                    Topicparent topic.id
        name =
            case maybeterm of
                Just term ->
                    "Docs with " ++ term.name
                _ ->
                    "Docs in Topic " ++ (toString topic.id)
    in
    loadData Document.bestDocsDecoder (NewDocs parent name slotId) command

loadTerms : Topic -> Int -> Int -> Cmd Msg
loadTerms topic offset slotId =
    let command =
            String.concat
                [ "getTerms&TopicId="
                , (toString topic.id)
                , "&offset="
                , (toString offset)
                ]
    in
    loadData Term.termsDecoder (NewTerms (Topicparent topic.id) ("Terms in Topic " ++ (toString topic.id)) slotId) command

loadBestTerms : Int -> Cmd Msg
loadBestTerms slotId =
    loadData Term.bestTermsDecoder (NewTerms (Noparent) "Terms" slotId) "getBestTerms"

loadAutocompleteTerms : Parent -> String  -> Int-> Cmd Msg
loadAutocompleteTerms parent termName slotId=
    let command = "autocomplete&SearchWord=" ++ termName
    in
    loadData Term.searchTermDecoder (NewTermTopics (parent) termName slotId) command

loadBestFrames : Int -> Cmd Msg
loadBestFrames slotId =
    loadData Term.bestTermsDecoder (NewFrames (Noparent) "Frames" slotId) "getBestFrames"

loadSearchTopics : String -> Cmd Msg
loadSearchTopics search =
    let command =
            "autocomplete&SearchWord=" ++ search
        name = "Search Result for " ++ search
    in
    loadData Term.searchTermDecoder (NewSearchTopics (Noparent) name) command

loadSearchTerms : String -> Cmd Msg
loadSearchTerms search =
    let command =
            "autocomplete&SearchWord=" ++ search
        name = "Search Result for " ++ search
    in
    loadData Term.searchTermDecoder (NewSearchTerms (Noparent) name) command

loadSearchDocs : String -> Bool -> String -> Cmd Msg
loadSearchDocs search strict sorting =
    let command =
            String.concat
                [ "search&SearchWord="
                , search
                , "&SearchStrict="
                , if strict
                    then "true"
                    else "false"
                , "&sorting="
                , sorting
                ]
        name = "Search Result for " ++ search
    in
    loadData Document.bestDocsDecoder (NewSearchDocs (Noparent) name) command

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
