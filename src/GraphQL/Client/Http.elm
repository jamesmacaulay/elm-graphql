module GraphQL.Client.Http
    exposing
        ( Error
        , sendOp
        , sendRaw
        )

import GraphQL.Query.Builder as QueryBuilder
import GraphQL.Query.Builder.Encode as QueryEncode
import Json.Decode
import Json.Encode
import Http
import Task exposing (Task)


type Error
    = HttpError Http.Error
    | InvalidQueryError (List QueryBuilder.BuilderError)


sendOp : String -> QueryBuilder.Op a -> Maybe Json.Encode.Value -> Task Error a
sendOp url op =
    case op |> QueryBuilder.getStructure |> QueryEncode.encodeQueryBuilder of
        Err errs ->
            always (Task.fail (InvalidQueryError errs))

        Ok documentString ->
            sendRaw url documentString (QueryBuilder.getDecoder op)
                >> Task.mapError HttpError


sendRaw : String -> String -> Json.Decode.Decoder a -> Maybe Json.Encode.Value -> Task Http.Error a
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

        responseDecoder =
            Json.Decode.at [ "data" ] decoder

        request =
            Http.post url body responseDecoder
    in
        Http.toTask request
