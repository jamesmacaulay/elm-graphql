module GraphQL.Schema.Decode
    exposing
        ( scalarTypeDecoder
        , objectTypeDecoder
        , unionTypeDecoder
        , interfaceTypeDecoder
        , enumTypeDecoder
        , inputObjectTypeDecoder
        , directiveDecoder
        , schemaDecoder
        , introspectionResponseDecoder
        )

import Json.Decode as Decode exposing (Decoder, (:=), succeed)
import GraphQL.Schema as Schema exposing (Schema)
import Dict exposing (Dict)


nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
    Decode.oneOf [ Decode.null Nothing, Decode.map Just decoder ]


construct : a -> Decoder a
construct =
    succeed


with : Decoder a -> Decoder (a -> b) -> Decoder b
with =
    Decode.object2 (|>)


fail : String -> Decoder a
fail msg =
    Decode.customDecoder (construct (Err msg)) identity


withNameAndDescription : Decoder (String -> Maybe String -> a) -> Decoder a
withNameAndDescription constructor =
    constructor
        |> with ("name" := Decode.string)
        |> with ("description" := nullable Decode.string)


scalarTypeDecoder : Decoder Schema.NamedType
scalarTypeDecoder =
    construct Schema.ScalarTypeInfo
        |> withNameAndDescription
        |> Decode.map Schema.ScalarType


objectTypeDecoder : Decoder Schema.NamedType
objectTypeDecoder =
    construct Schema.ObjectTypeInfo
        |> withNameAndDescription
        |> with ("fields" := Decode.list fieldDecoder)
        |> with ("interfaces" := Decode.list typeRefDecoder)
        |> Decode.map Schema.ObjectType


fieldDecoder : Decoder Schema.Field
fieldDecoder =
    construct Schema.Field
        |> withNameAndDescription
        |> with ("args" := Decode.list inputValueDecoder)
        |> with ("type" := typeRefDecoder)
        |> with ("isDeprecated" := Decode.bool)
        |> with ("deprecationReason" := nullable Decode.string)


inputValueDecoder : Decoder Schema.InputValue
inputValueDecoder =
    construct Schema.InputValue
        |> withNameAndDescription
        |> with ("type" := typeRefDecoder)
        |> with ("defaultValue" := nullable Decode.string)


unionTypeDecoder : Decoder Schema.NamedType
unionTypeDecoder =
    construct Schema.UnionTypeInfo
        |> withNameAndDescription
        |> with ("possibleTypes" := Decode.list typeRefDecoder)
        |> Decode.map Schema.UnionType


interfaceTypeDecoder : Decoder Schema.NamedType
interfaceTypeDecoder =
    construct Schema.InterfaceTypeInfo
        |> withNameAndDescription
        |> with ("fields" := Decode.list fieldDecoder)
        |> with ("possibleTypes" := Decode.list typeRefDecoder)
        |> Decode.map Schema.InterfaceType


enumTypeDecoder : Decoder Schema.NamedType
enumTypeDecoder =
    construct Schema.EnumTypeInfo
        |> withNameAndDescription
        |> with ("enumValues" := Decode.list enumValueDecoder)
        |> Decode.map Schema.EnumType


enumValueDecoder : Decoder Schema.EnumValue
enumValueDecoder =
    construct Schema.EnumValue
        |> withNameAndDescription
        |> with ("isDeprecated" := Decode.bool)
        |> with ("deprecationReason" := nullable Decode.string)


inputObjectTypeDecoder : Decoder Schema.NamedType
inputObjectTypeDecoder =
    construct Schema.InputObjectTypeInfo
        |> withNameAndDescription
        |> with ("inputFields" := Decode.list inputValueDecoder)
        |> Decode.map Schema.InputObjectType


typeRefDecoder : Decoder Schema.TypeRef
typeRefDecoder =
    ("kind" := Decode.string)
        |> (flip Decode.andThen)
            (\kind ->
                case kind of
                    "LIST" ->
                        ("ofType" := typeRefDecoder)
                            |> Decode.map Schema.List

                    "NON_NULL" ->
                        ("ofType" := typeRefDecoder)
                            |> Decode.map Schema.NonNull

                    _ ->
                        ("name" := Decode.string)
                            |> Decode.map Schema.Ref
            )


namedTypeDecoder : Decoder Schema.NamedType
namedTypeDecoder =
    ("kind" := Decode.string)
        |> (flip Decode.andThen)
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
                        fail ("unexpected kind for named type " ++ toString kind)
            )


namedTypeTupleDecoder : Decoder ( String, Schema.NamedType )
namedTypeTupleDecoder =
    construct (,)
        |> with ("name" := Decode.string)
        |> with namedTypeDecoder


typesDecoder : Decoder (Dict String Schema.NamedType)
typesDecoder =
    Decode.list namedTypeTupleDecoder
        |> Decode.map Dict.fromList


directiveLocationFromString : String -> Result String Schema.DirectiveLocation
directiveLocationFromString loc =
    case loc of
        "QUERY" ->
            Ok Schema.QueryLocation

        "MUTATION" ->
            Ok Schema.MutationLocation

        "FIELD" ->
            Ok Schema.FieldLocation

        "FRAGMENT_DEFINITION" ->
            Ok Schema.FragmentDefinitionLocation

        "FRAGMENT_SPREAD" ->
            Ok Schema.FragmentSpreadLocation

        "INLINE_FRAGMENT" ->
            Ok Schema.InlineFragmentLocation

        _ ->
            Err ("unexpected DirectiveLocation " ++ toString loc)


directiveLocationDecoder : Decoder Schema.DirectiveLocation
directiveLocationDecoder =
    Decode.customDecoder Decode.string directiveLocationFromString


directiveDecoder : Decoder Schema.Directive
directiveDecoder =
    construct Schema.Directive
        |> with ("name" := Decode.string)
        |> with ("description" := nullable Decode.string)
        |> with ("locations" := Decode.list directiveLocationDecoder)
        |> with ("args" := Decode.list inputValueDecoder)


schemaDecoder : Decoder Schema
schemaDecoder =
    construct Schema
        |> with (Decode.at [ "queryType", "name" ] Decode.string)
        |> with ("mutationType" := (nullable ("name" := Decode.string)))
        |> with ("subscriptionType" := (nullable ("name" := Decode.string)))
        |> with ("types" := typesDecoder)
        |> with ("directives" := Decode.list directiveDecoder)


introspectionResponseDecoder : Decoder Schema
introspectionResponseDecoder =
    Decode.at [ "data", "__schema" ] schemaDecoder
