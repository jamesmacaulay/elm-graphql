module GraphQL.Client.Http
    exposing
        ( queryRequest
        , mutationRequest
        , rawRequest
        )

import GraphQL.Request.Builder as Builder
import GraphQL.Request.Builder.Value as Value
import GraphQL.Request.Document.AST.Serialize as Serialize
import GraphQL.Request.Document.AST.Value.Json.Encode as ValueEncode
import GraphQL.Response as Response
import Json.Decode
import Json.Encode
import Http
import Tuple


variableValuesToJson : List ( String, Value.Constant ) -> Maybe Json.Encode.Value
variableValuesToJson kvPairs =
    if List.isEmpty kvPairs then
        Nothing
    else
        kvPairs
            |> List.map (Tuple.mapSecond ValueEncode.encode)
            |> Json.Encode.object
            |> Just


request :
    String
    -> Builder.Request operationType result
    -> Http.Request (Result (List Response.RequestError) result)
request url request =
    let
        documentString =
            request
                |> Builder.requestAST
                |> Serialize.serializeDocument

        decoder =
            Builder.responseDecoder request

        variableValuesJson =
            variableValuesToJson request.variableValues
    in
        rawRequest url documentString decoder variableValuesJson


queryRequest :
    String
    -> Builder.Request Builder.Query result
    -> Http.Request (Result (List Response.RequestError) result)
queryRequest =
    request


mutationRequest :
    String
    -> Builder.Request Builder.Mutation result
    -> Http.Request (Result (List Response.RequestError) result)
mutationRequest =
    request


rawRequest : String -> String -> Json.Decode.Decoder a -> Maybe Json.Encode.Value -> Http.Request a
rawRequest url documentString decoder variableValues =
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
