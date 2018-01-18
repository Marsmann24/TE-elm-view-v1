module Request exposing (..)

import Model exposing (Msg(..), Command(..))
import Topic
import Term
import Document

import Http
import Json.Decode exposing (Decoder)

baseURL : String
baseURL = "http://topicexplorer.informatik.uni-halle.de/09sdfjglikqw3bret5cp84vqyolrfiksefgdakyuheas/webapp/ZEIT0614_3_te/JsonServlet?Command="

loadData : Command -> String -> Cmd Msg
loadData command arguments =
    let url = (baseURL ++ (toString command)) ++ arguments
        request = Http.get url (getDecoder command)
    in
    Http.send (getMsg command) request

getDecoder : Command -> Decoder a
getDecoder command =
    case command of
        GetTopics ->
            Topic.decodeTopics
        GetDoc ->
            Document.documentDecoder
        GetBestDocs ->
            Document.bestDocsDecoder
        GetTerms ->
            Term.termsDecoder
        GetBestTerms ->
            Term.bestTermsDecoder
        GetBestFrames ->
            Term.bestTermsDecoder

getMsg : Command -> a -> Msg
getMsg command a = None
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
