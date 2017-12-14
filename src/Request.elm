module Request exposing (..)

import Topic

import Http
import Json.Decode exposing (Decoder)

baseURL : String
baseURL = "http://topicexplorer.informatik.uni-halle.de/09sdfjglikqw3bret5cp84vqyolrfiksefgdakyuheas/webapp/ZEIT0614_3_te/JsonServlet?Command="

loadData : Command -> Cmd Msg
loadData command =
    let url = baseURL ++ (toString command)
        request = Http.get url (getDecoder command)
    in
    Http.send JsonLoaded request

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
            getDocDecoder
        GetBestDocs ->
            getBestDocsDecoder
        GetBestTerms ->
            getBestTermsDecoder
        GetTerms ->
            getTermsDecoder
        GetBestFrames ->
            getBestFramesDecoder

getDocDecoder : Decoder a
getBestDocsDecoder : Decoder a
getBestTermsDecoder : Decoder a
getBestFramesDecoder : Decoder a
