module Request exposing (..)

import Topic
import Term
import Document

import Http
import Json.Decode exposing (Decoder)

baseURL : String
baseURL = "http://topicexplorer.informatik.uni-halle.de/09sdfjglikqw3bret5cp84vqyolrfiksefgdakyuheas/webapp/ZEIT0614_3_te/JsonServlet?Command="

loadData : Msg -> Command -> String -> Cmd Msg
loadData command arguments =
    let url = baseURL ++ (toString command) ++ arguments
        request = Http.get url (getDecoder command)
    in
    Http.send msg request

type Command
    = GetTopics
    | GetDoc
    | GetBestDocs
    | GetBestTerms
    | GetTerms
    | GetBestFrames

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
