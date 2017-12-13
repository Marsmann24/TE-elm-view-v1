module Request exposing (..)

import Http
import Decoder

baseURL : String
baseURL = "http://topicexplorer.informatik.uni-halle.de/09sdfjglikqw3bret5cp84vqyolrfiksefgdakyuheas/webapp/ZEIT0614_3_te/JsonServlet?Command="

loadData : String -> Cmd Msg
loadData command =
    let url = baseURL ++ command
        request = Http.get url decodeJson
    in
    Http.send JsonLoaded request


decodeJson : Decoder
