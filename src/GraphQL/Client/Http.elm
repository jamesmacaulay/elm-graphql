module GraphQL.Client.Http exposing (Error(..), RequestError, DocumentLocation, sendQuery, sendMutation, RequestOptions, customSendQuery, customSendMutation, customSendQueryRaw, customSendMutationRaw)

{-| The functions in this module let you perform HTTP requests to conventional GraphQL server endpoints.

@docs Error, RequestError, DocumentLocation, sendQuery, sendMutation, RequestOptions, customSendQuery, customSendMutation, customSendQueryRaw, customSendMutationRaw

-}

import GraphQL.Client.Http.Util as Util
import GraphQL.Request.Builder as Builder
import Http
import Task exposing (Task)


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
sendQuery =
    Util.defaultRequestOptions >> send


{-| Takes a URL and a `Query` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint and return raw `Http.Response` in Task.
-}
sendQueryRaw :
    String
    -> Builder.Request Builder.Query result
    -> Task Error (Http.Response String)
sendQueryRaw =
    Util.defaultRequestOptions >> sendExpecting rawExpect


{-| Takes a URL and a `Mutation` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint.
-}
sendMutation :
    String
    -> Builder.Request Builder.Mutation result
    -> Task Error result
sendMutation =
    Util.defaultRequestOptions >> send


{-| Takes a URL and a `Mutation` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint and return raw `Http.Response` in Task.
-}
sendMutationRaw :
    String
    -> Builder.Request Builder.Mutation result
    -> Task Error (Http.Response String)
sendMutationRaw =
    Util.defaultRequestOptions >> sendExpecting rawExpect


{-| Options available for customizing GraphQL HTTP requests. `method` should be either `"GET"` or `"POST"`. For `GET` requests, the `url` is modified to include extra parameters in the query string for the GraphQL document and variables. Otherwise, the document and variables are included in the HTTP request body.
-}
type alias RequestOptions =
    { method : String
    , headers : List Http.Header
    , url : String
    , timeout : Maybe Float
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
    sendExpecting rawExpect


{-| Like `sendMutation`, but takes an `RequestOptions` value instead of a URL to let you further customize the HTTP request.
-}
customSendMutation :
    RequestOptions
    -> Builder.Request Builder.Mutation result
    -> Task Error result
customSendMutation =
    send


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
    sendExpecting rawExpect


rawExpect : Http.Expect (Http.Response String)
rawExpect =
    Http.expectStringResponse Ok


send :
    RequestOptions
    -> Builder.Request operationType result
    -> Task Error result
send options request =
    let
        expect =
            Util.defaultExpect (Builder.responseDataDecoder request)
    in
    sendExpecting expect options request


sendExpecting :
    Http.Expect result
    -> RequestOptions
    -> Builder.Request operationType result2
    -> Task Error result
sendExpecting expect requestOptions request =
    let
        documentString =
            Builder.requestBody request

        variableValues =
            Builder.jsonVariableValues request
    in
    Util.requestConfig requestOptions documentString expect variableValues
        |> Http.request
        |> Http.toTask
        |> Task.mapError (Util.convertHttpError HttpError GraphQLError)
