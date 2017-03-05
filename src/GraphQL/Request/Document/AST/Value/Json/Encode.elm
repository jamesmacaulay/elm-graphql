module GraphQL.Request.Document.AST.Value.Json.Encode exposing (encode)

import GraphQL.Request.Document.AST as AST
import Json.Encode as Json
import Tuple


encode : AST.ConstantValue -> Json.Value
encode value =
    case value of
        AST.VariableValue _ _ ->
            Json.null

        AST.IntValue int ->
            Json.int int

        AST.FloatValue float ->
            Json.float float

        AST.StringValue string ->
            Json.string string

        AST.BooleanValue bool ->
            Json.bool bool

        AST.NullValue ->
            Json.null

        AST.EnumValue string ->
            Json.string string

        AST.ListValue values ->
            Json.list (List.map encode values)

        AST.ObjectValue kvPairs ->
            Json.object (List.map (Tuple.mapSecond encode) kvPairs)
