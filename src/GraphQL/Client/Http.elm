module GraphQL.Client.Http
    exposing
        ( Error(..)
        , sendQuery
        , sendMutation
        , sendRaw
        )

{-| The functions in this module let you perform HTTP requests to conventional GraphQL server endpoints.

@docs Error, sendQuery, sendMutation, sendRaw
-}

import GraphQL.Request.Builder as Builder
import GraphQL.Request.Document.AST as AST
import GraphQL.Request.Document.AST.Value.Json.Encode as ValueEncode
import GraphQL.Response as Response
import Json.Decode
import Json.Encode
import Http
import Tuple
import Task exposing (Task)


{-| Represents errors that can occur when sending a GraphQL request over HTTP.
-}
type Error
    = HttpError Http.Error
    | GraphQLError (List Response.Error)


variableValuesToJson : List ( String, AST.ConstantValue ) -> Maybe Json.Encode.Value
variableValuesToJson kvPairs =
    if List.isEmpty kvPairs then
        Nothing
    else
        kvPairs
            |> List.map (Tuple.mapSecond ValueEncode.encode)
            |> Json.Encode.object
            |> Just


send :
    String
    -> Builder.Request operationType result
    -> Task Error result
send url request =
    let
        documentString =
            Builder.requestBody request

        decoder =
            Builder.responseDecoder request

        variableValuesJson =
            request
                |> Builder.requestVariableValues
                |> variableValuesToJson
    in
        sendRaw url documentString decoder variableValuesJson


{-| Takes a URL and a `Query` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send the request to a GraphQL server at the given endpoint.
-}
sendQuery :
    String
    -> Builder.Request Builder.Query result
    -> Task Error result
sendQuery =
    send


{-| Takes a URL and a `Mutation` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send the request to a GraphQL server at the given endpoint.
-}
sendMutation :
    String
    -> Builder.Request Builder.Mutation result
    -> Task Error result
sendMutation =
    send


handleErrorWithResponseBody : Http.Error -> String -> Error
handleErrorWithResponseBody error responseBody =
    responseBody
        |> Json.Decode.decodeString Response.errorsDecoder
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


{-| Returns a `Task` to send a GraphQL request built out of its component pieces. Takes a URL, a serialized GraphQL request document, a `Json.Decode.Decoder` for the response body, and an optional `Json.Encode.Value` for variable values.
-}
sendRaw : String -> String -> Json.Decode.Decoder a -> Maybe Json.Encode.Value -> Task Error a
sendRaw url documentString decoder variableValues =
    let
        documentValue =
            Json.Encode.string documentString

        extraParams =
            variableValues
                |> Maybe.map (\obj -> [ ( "variables", obj ) ])
                |> Maybe.withDefault []

        params =
            Json.Encode.object ([ ( "query", documentValue ) ] ++ extraParams)

        body =
            Http.jsonBody params
    in
        Http.post url body decoder
            |> Http.toTask
            |> Task.mapError convertHttpError
