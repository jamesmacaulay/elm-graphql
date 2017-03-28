module GraphQL.Client.Http
    exposing
        ( RequestError
        , DocumentLocation
        , Error(..)
        , RequestOptions
        , sendQuery
        , sendMutation
        , customSendQuery
        , customSendMutation
        )

{-| The functions in this module let you perform HTTP requests to conventional GraphQL server endpoints.

@docs Error, RequestError, DocumentLocation, sendQuery, sendMutation, RequestOptions,  customSendQuery, customSendMutation
-}

import GraphQL.Request.Builder as Builder
import GraphQL.Response as Response
import Json.Decode
import Json.Encode
import Http
import Task exposing (Task)
import Time exposing (Time)
import String


{-| An error returned by the GraphQL server that indicates there was something wrong with the request.
-}
type alias RequestError =
    { message : String
    , locations : List DocumentLocation
    }


{-| A location in a GraphQL request document.
-}
type alias DocumentLocation =
    { line : Int
    , column : Int
    }


{-| Represents errors that can occur when sending a GraphQL request over HTTP.
-}
type Error
    = HttpError Http.Error
    | GraphQLError (List RequestError)


send :
    RequestOptions
    -> Builder.Request operationType result
    -> Task Error result
send requestOptions request =
    let
        documentString =
            Builder.requestBody request

        decoder =
            Builder.responseDataDecoder request

        variableValuesJson =
            Builder.jsonVariableValues request
    in
        sendRaw requestOptions documentString decoder variableValuesJson


{-| Takes a URL and a `Query` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint.
-}
sendQuery :
    String
    -> Builder.Request Builder.Query result
    -> Task Error result
sendQuery =
    defaultRequestOptions >> send


{-| Takes a URL and a `Mutation` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint.
-}
sendMutation :
    String
    -> Builder.Request Builder.Mutation result
    -> Task Error result
sendMutation =
    defaultRequestOptions >> send


{-| Options available for customizing GraphQL HTTP requests. `method` should be either `"GET"` or `"POST"`. For `GET` requests, the `url` is modified to include extra parameters in the query string for the GraphQL document and variables. Otherwise, the document and variables are included in the HTTP request body.
-}
type alias RequestOptions =
    { method : String
    , headers : List Http.Header
    , url : String
    , timeout : Maybe Time
    , withCredentials : Bool
    }


{-| Like `sendQuery`, but takes an `RequestOptions` value instead of a URL to let you further customize the HTTP request.
-}
customSendQuery :
    RequestOptions
    -> Builder.Request Builder.Query result
    -> Task Error result
customSendQuery =
    send


{-| Like `sendMutation`, but takes an `RequestOptions` value instead of a URL to let you further customize the HTTP request.
-}
customSendMutation :
    RequestOptions
    -> Builder.Request Builder.Mutation result
    -> Task Error result
customSendMutation =
    send


defaultRequestOptions : String -> RequestOptions
defaultRequestOptions url =
    { method = "POST"
    , headers = []
    , url = url
    , timeout = Nothing
    , withCredentials = False
    }


errorsResponseDecoder : Json.Decode.Decoder (List RequestError)
errorsResponseDecoder =
    Json.Decode.field "errors" Response.errorsDecoder


handleErrorWithResponseBody : Http.Error -> String -> Error
handleErrorWithResponseBody error responseBody =
    responseBody
        |> Json.Decode.decodeString errorsResponseDecoder
        |> Result.map GraphQLError
        |> Result.withDefault (HttpError error)


convertHttpError : Http.Error -> Error
convertHttpError error =
    case error of
        Http.BadStatus { body } ->
            handleErrorWithResponseBody error body

        Http.BadPayload _ { body } ->
            handleErrorWithResponseBody error body

        _ ->
            HttpError error


postBody : String -> Maybe Json.Encode.Value -> Http.Body
postBody documentString variableValues =
    let
        documentValue =
            Json.Encode.string documentString

        extraParams =
            variableValues
                |> Maybe.map (\obj -> [ ( "variables", obj ) ])
                |> Maybe.withDefault []

        params =
            Json.Encode.object ([ ( "query", documentValue ) ] ++ extraParams)
    in
        Http.jsonBody params


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


sendRaw :
    RequestOptions
    -> String
    -> Json.Decode.Decoder a
    -> Maybe Json.Encode.Value
    -> Task Error a
sendRaw { method, headers, url, timeout, withCredentials } documentString dataDecoder variableValues =
    let
        decoder =
            Json.Decode.field "data" dataDecoder

        ( finalUrl, body ) =
            if method == "GET" then
                ( parameterizedUrl url documentString variableValues, Http.emptyBody )
            else
                ( url, postBody documentString variableValues )

        requestConfig =
            { method = method
            , headers = headers
            , url = finalUrl
            , body = body
            , expect = Http.expectJson decoder
            , timeout = timeout
            , withCredentials = withCredentials
            }
    in
        Http.request requestConfig
            |> Http.toTask
            |> Task.mapError convertHttpError
