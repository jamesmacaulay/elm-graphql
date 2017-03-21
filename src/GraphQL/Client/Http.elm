module GraphQL.Client.Http
    exposing
        ( RequestError
        , DocumentLocation
        , Error(..)
        , sendQuery
        , sendQueryWithHeaders
        , sendMutation
        , sendMutationWithHeaders
        )

{-| The functions in this module let you perform HTTP requests to conventional GraphQL server endpoints.

@docs Error, RequestError, DocumentLocation, sendQuery, sendMutation
-}

import GraphQL.Request.Builder as Builder
import GraphQL.Response as Response
import Json.Decode
import Json.Encode
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


send :
    String
    -> List (Http.Header)
    -> Builder.Request operationType result
    -> Task Error result
send url headers request =
    let
        documentString =
            Builder.requestBody request

        decoder =
            Builder.responseDataDecoder request

        variableValuesJson =
            Builder.jsonVariableValues request
    in
        sendRaw url headers documentString decoder variableValuesJson


{-| Takes a URL and a `Query` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send the request to a GraphQL server at the given endpoint.
-}
sendQuery :
    String
    -> Builder.Request Builder.Query result
    -> Task Error result
sendQuery url request =
    send url [] request


{-| Takes a URL, a list of `Http.Header`s and a `Query` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send the request to a GraphQL server at the given endpoint.
-}
sendQueryWithHeaders :
    String
    -> List (Http.Header)
    -> Builder.Request Builder.Query result
    -> Task Error result
sendQueryWithHeaders =
    send


{-| Takes a URL and a `Mutation` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send the request to a GraphQL server at the given endpoint.
-}
sendMutation :
    String
    -> Builder.Request Builder.Mutation result
    -> Task Error result
sendMutation url request =
    send url [] request


{-| Takes a URL, a list of `Http.Header`s and a `Mutation` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send the request to a GraphQL server at the given endpoint.
-}
sendMutationWithHeaders :
    String
    -> List (Http.Header)
    -> Builder.Request Builder.Mutation result
    -> Task Error result
sendMutationWithHeaders =
    send


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


sendRaw : String -> List (Http.Header) -> String -> Json.Decode.Decoder a -> Maybe Json.Encode.Value -> Task Error a
sendRaw url headers documentString dataDecoder variableValues =
    let
        documentValue =
            Json.Encode.string documentString

        decoder =
            Json.Decode.field "data" dataDecoder

        extraParams =
            variableValues
                |> Maybe.map (\obj -> [ ( "variables", obj ) ])
                |> Maybe.withDefault []

        params =
            Json.Encode.object ([ ( "query", documentValue ) ] ++ extraParams)

        body =
            Http.jsonBody params
    in
        Http.request
            { method = "POST"
            , headers = headers
            , url = url
            , body = body
            , expect = Http.expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }
            |> Http.toTask
            |> Task.mapError convertHttpError
