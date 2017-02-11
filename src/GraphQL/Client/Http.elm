module GraphQL.Client.Http
    exposing
        ( opRequest
        , rawRequest
        )

import GraphQL.Query.Builder as QueryBuilder
import GraphQL.Query.Builder.Encode as QueryEncode
import Json.Decode
import Json.Encode
import Http


opRequest : String -> QueryBuilder.Op a -> Maybe Json.Encode.Value -> Http.Request a
opRequest url op =
    let
        documentString =
            op
                |> QueryBuilder.getStructure
                |> QueryEncode.encodeOp

        decoder =
            QueryBuilder.getDecoder op
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
