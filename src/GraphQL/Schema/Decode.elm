module GraphQL.Schema.Decode exposing
    ( directiveDecoder
    , enumTypeDecoder
    , inputObjectTypeDecoder
    , interfaceTypeDecoder
    , introspectionResponseDecoder
    , objectTypeDecoder
    , scalarTypeDecoder
    , schemaDecoder
    , unionTypeDecoder
    )

import Dict exposing (Dict)
import GraphQL.Schema as Schema exposing (Schema)
import Json.Decode as Decode exposing (Decoder, bool, field, list, null, string)
import Json.Encode as Encode


nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
    Decode.oneOf [ null Nothing, Decode.map Just decoder ]


construct : a -> Decoder a
construct =
    Decode.succeed


with : Decoder a -> Decoder (a -> b) -> Decoder b
with =
    Decode.map2 (|>)


withNameAndDescription : Decoder (String -> Maybe String -> a) -> Decoder a
withNameAndDescription constructor =
    constructor
        |> with (field "name" string)
        |> with (field "description" (nullable string))


scalarTypeDecoder : Decoder Schema.NamedType
scalarTypeDecoder =
    construct Schema.ScalarTypeInfo
        |> withNameAndDescription
        |> Decode.map Schema.ScalarType


objectTypeDecoder : Decoder Schema.NamedType
objectTypeDecoder =
    construct Schema.ObjectTypeInfo
        |> withNameAndDescription
        |> with (field "fields" (list fieldDecoder))
        |> with (field "interfaces" (list typeRefDecoder))
        |> Decode.map Schema.ObjectType


fieldDecoder : Decoder Schema.Field
fieldDecoder =
    construct Schema.Field
        |> withNameAndDescription
        |> with (field "args" (list inputValueDecoder))
        |> with (field "type" typeRefDecoder)
        |> with (field "isDeprecated" bool)
        |> with (field "deprecationReason" (nullable string))


inputValueDecoder : Decoder Schema.InputValue
inputValueDecoder =
    construct Schema.InputValue
        |> withNameAndDescription
        |> with (field "type" typeRefDecoder)
        |> with (field "defaultValue" (nullable string))


unionTypeDecoder : Decoder Schema.NamedType
unionTypeDecoder =
    construct Schema.UnionTypeInfo
        |> withNameAndDescription
        |> with (field "possibleTypes" (list typeRefDecoder))
        |> Decode.map Schema.UnionType


interfaceTypeDecoder : Decoder Schema.NamedType
interfaceTypeDecoder =
    construct Schema.InterfaceTypeInfo
        |> withNameAndDescription
        |> with (field "fields" (list fieldDecoder))
        |> with (field "possibleTypes" (list typeRefDecoder))
        |> Decode.map Schema.InterfaceType


enumTypeDecoder : Decoder Schema.NamedType
enumTypeDecoder =
    construct Schema.EnumTypeInfo
        |> withNameAndDescription
        |> with (field "enumValues" (list enumValueDecoder))
        |> Decode.map Schema.EnumType


enumValueDecoder : Decoder Schema.EnumValue
enumValueDecoder =
    construct Schema.EnumValue
        |> withNameAndDescription
        |> with (field "isDeprecated" bool)
        |> with (field "deprecationReason" (nullable string))


inputObjectTypeDecoder : Decoder Schema.NamedType
inputObjectTypeDecoder =
    construct Schema.InputObjectTypeInfo
        |> withNameAndDescription
        |> with (field "inputFields" (list inputValueDecoder))
        |> Decode.map Schema.InputObjectType


typeRefDecoder : Decoder Schema.TypeRef
typeRefDecoder =
    field "kind" string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "LIST" ->
                        field "ofType" typeRefDecoder
                            |> Decode.map Schema.List

                    "NON_NULL" ->
                        field "ofType" typeRefDecoder
                            |> Decode.map Schema.NonNull

                    _ ->
                        field "name" string
                            |> Decode.map Schema.Ref
            )


namedTypeDecoder : Decoder Schema.NamedType
namedTypeDecoder =
    field "kind" string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "SCALAR" ->
                        scalarTypeDecoder

                    "OBJECT" ->
                        objectTypeDecoder

                    "UNION" ->
                        unionTypeDecoder

                    "INTERFACE" ->
                        interfaceTypeDecoder

                    "ENUM" ->
                        enumTypeDecoder

                    "INPUT_OBJECT" ->
                        inputObjectTypeDecoder

                    _ ->
                        Decode.fail ("unexpected kind for named type " ++ Encode.encode 0 (Encode.string kind))
            )


namedTypeTupleDecoder : Decoder ( String, Schema.NamedType )
namedTypeTupleDecoder =
    construct (\a b -> ( a, b ))
        |> with (field "name" string)
        |> with namedTypeDecoder


typesDecoder : Decoder (Dict String Schema.NamedType)
typesDecoder =
    list namedTypeTupleDecoder
        |> Decode.map Dict.fromList


decoderFromDirectiveLocation : String -> Decoder Schema.DirectiveLocation
decoderFromDirectiveLocation loc =
    case loc of
        "QUERY" ->
            Decode.succeed Schema.QueryLocation

        "MUTATION" ->
            Decode.succeed Schema.MutationLocation

        "FIELD" ->
            Decode.succeed Schema.FieldLocation

        "FRAGMENT_DEFINITION" ->
            Decode.succeed Schema.FragmentDefinitionLocation

        "FRAGMENT_SPREAD" ->
            Decode.succeed Schema.FragmentSpreadLocation

        "INLINE_FRAGMENT" ->
            Decode.succeed Schema.InlineFragmentLocation

        _ ->
            Decode.fail ("unexpected DirectiveLocation " ++ Encode.encode 0 (Encode.string loc))


directiveLocationDecoder : Decoder Schema.DirectiveLocation
directiveLocationDecoder =
    string |> Decode.andThen decoderFromDirectiveLocation


directiveDecoder : Decoder Schema.Directive
directiveDecoder =
    construct Schema.Directive
        |> with (field "name" string)
        |> with (field "description" (nullable string))
        |> with (field "locations" (list directiveLocationDecoder))
        |> with (field "args" (list inputValueDecoder))


schemaDecoder : Decoder Schema
schemaDecoder =
    construct Schema
        |> with (Decode.at [ "queryType", "name" ] string)
        |> with (field "mutationType" (nullable (field "name" string)))
        |> with (field "subscriptionType" (nullable (field "name" string)))
        |> with (field "types" typesDecoder)
        |> with (field "directives" (list directiveDecoder))


introspectionResponseDecoder : Decoder Schema
introspectionResponseDecoder =
    Decode.at [ "data", "__schema" ] schemaDecoder
