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

import GraphQL.Client.Http.Util as Util
import GraphQL.Request.Builder as Builder
import GraphQL.Response as Response
import Json.Decode
import Json.Encode
import Http
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
sendQuery =
    Util.defaultRequestOptions >> send


{-| Takes a URL and a `Mutation` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint.
-}
sendMutation :
    String
    -> Builder.Request Builder.Mutation result
    -> Task Error result
sendMutation =
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

        variableValues =
            Builder.jsonVariableValues request
    in
        Util.requestConfig requestOptions documentString decoder variableValues
            |> Http.request
            |> Http.toTask
            |> Task.mapError (Util.convertHttpError HttpError GraphQLError)
