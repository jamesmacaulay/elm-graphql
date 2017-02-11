module GraphQL.Client.Http
    exposing
        ( queryRequest
        , mutationRequest
        , rawRequest
        )

import GraphQL.Query.Builder as QueryBuilder
import GraphQL.Query.Builder.Encode as QueryEncode
import Json.Decode
import Json.Encode
import Http


queryRequest : String -> QueryBuilder.Query a -> Maybe Json.Encode.Value -> Http.Request a
queryRequest url query =
    let
        documentString =
            query
                |> QueryBuilder.getStructure
                |> QueryEncode.encodeQuery

        decoder =
            QueryBuilder.getDecoder query
    in
        rawRequest url documentString decoder


mutationRequest : String -> QueryBuilder.Mutation a -> Maybe Json.Encode.Value -> Http.Request a
mutationRequest url mutation =
    let
        documentString =
            mutation
                |> QueryBuilder.getStructure
                |> QueryEncode.encodeMutation

        decoder =
            QueryBuilder.getDecoder mutation
    in
        rawRequest url documentString decoder


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

        responseDecoder =
            Json.Decode.at [ "data" ] decoder
    in
        Http.post url body responseDecoder
