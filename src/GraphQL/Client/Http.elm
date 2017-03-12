module GraphQL.Client.Http
    exposing
        ( Error
        , sendQuery
        , sendMutation
        , sendRaw
        )

import GraphQL.Request.Builder as Builder
import GraphQL.Request.Document.AST as AST
import GraphQL.Request.Document.AST.Value.Json.Encode as ValueEncode
import GraphQL.Response as Response
import Json.Decode
import Json.Encode
import Http
import Tuple
import Task exposing (Task)


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
    -> Builder.Request operationType variableSource result
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


sendQuery :
    String
    -> Builder.Request Builder.Query variableSource result
    -> Task Error result
sendQuery =
    send


sendMutation :
    String
    -> Builder.Request Builder.Mutation variableSource result
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
