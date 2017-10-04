module GraphQL.Client.Http.Util exposing (..)

import GraphQL.Response as Response
import Http
import Json.Decode
import Json.Encode
import Time exposing (Time)


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
            firstParamPrefix ++ "query=" ++ Http.encodeUri documentString

        variablesParam =
            variableValues
                |> Maybe.map
                    (\obj ->
                        "&variables=" ++ Http.encodeUri (Json.Encode.encode 0 obj)
                    )
                |> Maybe.withDefault ""
    in
        url ++ queryParam ++ variablesParam


type alias RequestOptions =
    { method : String
    , headers : List Http.Header
    , url : String
    , timeout : Maybe Time
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


type Error
    = HttpError Http.Error
    | GraphQLError (List RequestError)


type alias RequestConfig =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect (Http.Response String)
    , timeout : Maybe Time
    , withCredentials : Bool
    }


defaultRequestOptions : String -> RequestOptions
defaultRequestOptions url =
    { method = "POST"
    , headers = []
    , url = url
    , timeout = Nothing
    , withCredentials = False
    }


requestConfig :
    RequestOptions
    -> String
    -> Maybe Json.Encode.Value
    -> RequestConfig
requestConfig requestOptions documentString variableValues =
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
        , expect = Http.expectStringResponse (\val -> Ok val)
        , timeout = requestOptions.timeout
        , withCredentials = requestOptions.withCredentials
        }


errorsResponseDecoder : Json.Decode.Decoder (List RequestError)
errorsResponseDecoder =
    Json.Decode.field "errors" Response.errorsDecoder


convertHttpError : (Http.Error -> err) -> (List RequestError -> err) -> Http.Error -> err
convertHttpError wrapHttpError wrapGraphQLError httpError =
    let
        handleErrorWithResponseBody responseBody =
            responseBody
                |> Json.Decode.decodeString errorsResponseDecoder
                |> Result.map wrapGraphQLError
                |> Result.withDefault (wrapHttpError httpError)
    in
        case httpError of
            Http.BadStatus { body } ->
                handleErrorWithResponseBody body

            Http.BadPayload _ { body } ->
                handleErrorWithResponseBody body

            _ ->
                wrapHttpError httpError
