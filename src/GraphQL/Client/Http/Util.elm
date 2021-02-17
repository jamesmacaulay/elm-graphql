module GraphQL.Client.Http.Util exposing (..)

import GraphQL.Response as Response
import Http
import Json.Decode
import Json.Encode as Encode
import Url


postBodyJsonWith : List ( String, Encode.Value ) -> String -> Maybe Encode.Value -> Encode.Value
postBodyJsonWith extraFields documentString variableValues =
    let
        documentValue =
            Encode.string documentString

        extraParams =
            case variableValues of
                Just obj ->
                    ( "variables", obj ) :: extraFields

                Nothing ->
                    extraFields
    in
    Encode.object <| ( "query", documentValue ) :: extraParams


postBodyJson : String -> Maybe Encode.Value -> Encode.Value
postBodyJson =
    postBodyJsonWith []


postBody : String -> Maybe Encode.Value -> Http.Body
postBody documentString variableValues =
    Http.jsonBody (postBodyJson documentString variableValues)


parameterizedUrl : String -> String -> Maybe Encode.Value -> String
parameterizedUrl url documentString variableValues =
    let
        firstParamPrefix =
            if String.contains "?" url then
                "&"

            else
                "?"

        queryParam =
            firstParamPrefix ++ "query=" ++ Url.percentEncode documentString

        variablesParam =
            variableValues
                |> Maybe.map
                    (\obj ->
                        "&variables=" ++ Url.percentEncode (Encode.encode 0 obj)
                    )
                |> Maybe.withDefault ""
    in
    url ++ queryParam ++ variablesParam


type alias RequestOptions =
    { method : String
    , headers : List Http.Header
    , url : String
    , timeout : Maybe Float
    , tracker : Maybe String
    , withCredentials : Bool
    }


type alias RequestError =
    { message : String
    , locations : List DocumentLocation
    }


type alias DocumentLocation =
    { line : Int
    , column : Int
    }


type alias RequestConfig a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect a
    , timeout : Maybe Float
    , tracker : Maybe String
    , withCredentials : Bool
    }


defaultRequestOptions : String -> RequestOptions
defaultRequestOptions url =
    { method = "POST"
    , headers = []
    , url = url
    , timeout = Nothing
    , tracker = Nothing
    , withCredentials = False
    }


requestConfig :
    RequestOptions
    -> String
    -> Http.Expect a
    -> Maybe Encode.Value
    -> RequestConfig a
requestConfig requestOptions documentString expect variableValues =
    let
        ( url, body ) =
            if requestOptions.method == "GET" then
                ( parameterizedUrl requestOptions.url documentString variableValues, Http.emptyBody )

            else
                ( requestOptions.url, postBody documentString variableValues )
    in
    { method = requestOptions.method
    , headers = requestOptions.headers
    , url = url
    , body = body
    , expect = expect
    , timeout = requestOptions.timeout
    , tracker = requestOptions.tracker
    , withCredentials = requestOptions.withCredentials
    }
