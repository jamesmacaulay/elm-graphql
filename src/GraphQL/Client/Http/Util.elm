module GraphQL.Client.Http.Util exposing (DocumentLocation, Error(..), RequestConfig, RequestError, RequestOptions, defaultRequestOptions, parameterizedUrl, postBody, postBodyJson, requestConfig)

import GraphQL.Response as Response
import Http
import Json.Decode
import Json.Encode
import Url


postBodyJson : String -> Maybe Json.Encode.Value -> Json.Encode.Value
postBodyJson documentString variableValues =
    let
        documentValue =
            Json.Encode.string documentString

        extraParams =
            variableValues
                |> Maybe.map (\obj -> [ ( "variables", obj ) ])
                |> Maybe.withDefault []
    in
    Json.Encode.object ([ ( "query", documentValue ) ] ++ extraParams)


postBody : String -> Maybe Json.Encode.Value -> Http.Body
postBody documentString variableValues =
    Http.jsonBody (postBodyJson documentString variableValues)


parameterizedUrl : String -> String -> Maybe Json.Encode.Value -> String
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
                        "&variables=" ++ Url.percentEncode (Json.Encode.encode 0 obj)
                    )
                |> Maybe.withDefault ""
    in
    url ++ queryParam ++ variablesParam


type alias RequestOptions =
    { method : String
    , headers : List Http.Header
    , url : String
    , timeout : Maybe Float
    }


type alias RequestError =
    { message : String
    , locations : List DocumentLocation
    }


type alias DocumentLocation =
    { line : Int
    , column : Int
    }


type Error
    = HttpError Http.Error
    | GraphQLError (List RequestError)


type alias RequestConfig a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect a
    , timeout : Maybe Float
    , tracker : Maybe String
    }


defaultRequestOptions : String -> RequestOptions
defaultRequestOptions url =
    { method = "POST"
    , headers = []
    , url = url
    , timeout = Nothing
    }


requestConfig :
    RequestOptions
    -> String
    -> Http.Expect a
    -> Maybe Json.Encode.Value
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
    , tracker = Nothing
    }
