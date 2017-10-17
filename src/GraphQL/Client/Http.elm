module GraphQL.Client.Http
    exposing
        ( RequestError
        , DocumentLocation
        , Error(..)
        , RequestOptions
        , sendQuery
        , sendMutation
        , customSendQuery
        , customSendQueryRaw
        , customSendMutation
        , customSendMutationRaw
        )

{-| The functions in this module let you perform HTTP requests to conventional GraphQL server endpoints.

@docs Error, RequestError, DocumentLocation, sendQuery, sendMutation, RequestOptions,  customSendQuery, customSendMutation, customSendQueryRaw, customSendMutationRaw
-}

import GraphQL.Client.Http.Util as Util
import GraphQL.Request.Builder as Builder
import GraphQL.Response as Response
import Http
import Json.Decode
import Json.Encode
import Task exposing (Task)
import Time exposing (Time)


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


{-| Takes a URL and a `Query` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint.
-}
sendQuery :
    String
    -> Builder.Request Builder.Query result
    -> Task Error result
sendQuery url request =
    parseResponse request <| sendQueryRaw url request


{-| Takes a URL and a `Query` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint and return raw `Http.Response` in Task.
-}
sendQueryRaw :
    String
    -> Builder.Request Builder.Query result
    -> Task Error (Http.Response String)
sendQueryRaw =
    Util.defaultRequestOptions >> send


{-| Takes a URL and a `Mutation` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint.
-}
sendMutation :
    String
    -> Builder.Request Builder.Mutation result
    -> Task Error result
sendMutation url request =
    parseResponse request <| sendMutationRaw url request


{-| Takes a URL and a `Mutation` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint and return raw `Http.Response` in Task.
-}
sendMutationRaw :
    String
    -> Builder.Request Builder.Mutation result
    -> Task Error (Http.Response String)
sendMutationRaw =
    Util.defaultRequestOptions >> send


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
customSendQuery options request =
    parseResponse request <| customSendQueryRaw options request


{-| Like `sendQuery`, but takes an `RequestOptions` value instead of a URL to let you further customize the HTTP request. You will get a plain `Http.Response` as Task result.

Useful for things like caching, custom errors decoding, etc.

Example of response decoding:

    let
        decoder =
            GraphQL.Request.Builder.responseDataDecoder request
                |> Json.Decode.field "data"

        options =
            { method = "GET"
            , headers = []
            , url = "/graphql"
            , timeout = Nothing
            , withCredentials = False
            }
    in
        request
            |> GraphQL.Client.Http.customSendQueryRaw options
            |> Task.andThen
                (\response ->
                    case Json.Decode.decodeString decoder response.body of
                        Err err ->
                            Task.fail <| GraphQL.Client.Http.HttpError <| Http.BadPayload err response

                        Ok decodedValue ->
                            Task.succeed decodedValue
                )
-}
customSendQueryRaw :
    RequestOptions
    -> Builder.Request Builder.Query result
    -> Task Error (Http.Response String)
customSendQueryRaw =
    send


{-| Like `sendMutation`, but takes an `RequestOptions` value instead of a URL to let you further customize the HTTP request.
-}
customSendMutation :
    RequestOptions
    -> Builder.Request Builder.Mutation result
    -> Task Error result
customSendMutation options request =
    parseResponse request <| customSendMutationRaw options request


{-| Like `sendMutation`, but takes an `RequestOptions` value instead of a URL to let you further customize the HTTP request. You will get a plain `Http.Response` as Task result.

Useful for things like custom errors decoding, etc.

Example of response decoding:

    let
        decoder =
            GraphQL.Request.Builder.responseDataDecoder mutationRequest
                |> Json.Decode.field "data"

        options =
            { method = "GET"
            , headers = []
            , url = "/graphql"
            , timeout = Nothing
            , withCredentials = False
            }
    in
        mutationRequest
            |> GraphQL.Client.Http.customSendMutationRaw options
            |> Task.andThen
                (\response ->
                    case Json.Decode.decodeString decoder response.body of
                        Err err ->
                            Task.fail <| GraphQL.Client.Http.HttpError <| Http.BadPayload err response

                        Ok decodedValue ->
                            Task.succeed decodedValue
                )

-}
customSendMutationRaw :
    RequestOptions
    -> Builder.Request Builder.Mutation result
    -> Task Error (Http.Response String)
customSendMutationRaw =
    send


parseResponse request response =
    let
        decoder =
            Builder.responseDataDecoder request
    in
        response
            |> Task.andThen (\response ->
                                 case Json.Decode.decodeString decoder response.body of
                                     Err err ->
                                         Task.fail <| HttpError <| Http.BadPayload err response
                                     Ok decodedValue ->
                                         Task.succeed decodedValue
                            )


send :
    RequestOptions
    -> Builder.Request operationType result
    -> Task Error (Http.Response String)
send requestOptions request =
    let
        documentString =
            Builder.requestBody request

        variableValues =
            Builder.jsonVariableValues request
    in
        Util.requestConfig requestOptions documentString variableValues
            |> Http.request
            |> Http.toTask
            |> Task.mapError (Util.convertHttpError HttpError GraphQLError)
