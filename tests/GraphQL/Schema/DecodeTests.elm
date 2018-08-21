module GraphQL.Schema.DecodeTests exposing (..)

import Test exposing (..)
import Expect
import Json.Decode as Decode
import GraphQL.Schema as Schema exposing (Schema)
import GraphQL.Schema.Decode
import String
import Dict exposing (Dict)


scalarTypeJSON =
    """
    {
        "kind": "SCALAR",
        "name": "Boolean",
        "description": "The `Boolean` scalar type represents `true` or `false`."
    }
    """


objectTypeJSON =
    """
    {
        "kind": "OBJECT",
        "name": "Thing",
        "description": "A test thing.",
        "fields": [
            {
                "name": "mass",
                "description": "mass of the thing",
                "args": [
                    {
                        "name": "inPounds",
                        "description": "get the mass in pounds",
                        "type": {
                            "kind": "SCALAR",
                            "name": "Boolean",
                            "ofType": null
                        },
                        "defaultValue": "false"
                    }
                ],
                "type": {
                    "kind": "NON_NULL",
                    "name": null,
                    "ofType": {
                        "kind": "SCALAR",
                        "name": "Float",
                        "ofType": null
                    }
                },
                "isDeprecated": true,
                "deprecationReason": "no good reason"
            }
        ],
        "interfaces": [
            {
                "kind": "INTERFACE",
                "name": "Massive",
                "ofType": null
            }
        ]
    }
    """


unionTypeJSON =
    """
    {
        "kind": "UNION",
        "name": "OnlyThing",
        "description": "A union of Thing and nothing else.",
        "possibleTypes": [
            {
                "kind": "OBJECT",
                "name": "Thing",
                "ofType": null
            }
        ]
    }
    """


interfaceTypeJSON =
    """
    {
        "kind": "INTERFACE",
        "name": "HasMass",
        "description": "has a mass",
        "fields": [
            {
                "name": "mass",
                "description": "mass of the thing",
                "args": [
                    {
                        "name": "inPounds",
                        "description": "get the mass in pounds",
                        "type": {
                            "kind": "SCALAR",
                            "name": "Boolean",
                            "ofType": null
                        },
                        "defaultValue": "false"
                    }
                ],
                "type": {
                    "kind": "NON_NULL",
                    "name": null,
                    "ofType": {
                        "kind": "SCALAR",
                        "name": "Float",
                        "ofType": null
                    }
                },
                "isDeprecated": true,
                "deprecationReason": "no good reason"
            }
        ],
        "possibleTypes": [
            {
                "kind": "OBJECT",
                "name": "Thing",
                "ofType": null
            }
        ]
    }
    """


enumTypeJSON =
    """
    {
        "kind": "ENUM",
        "name": "UnitSystem",
        "description": "a system of units",
        "enumValues": [
            {
                "name": "METRIC",
                "description": "a very popular unit system",
                "isDeprecated": false,
                "deprecationReason": null
            },
            {
                "name": "IMPERIAL",
                "description": null,
                "isDeprecated": true,
                "deprecationReason": "it's a bit weird"
            }
        ]
    }
    """


inputObjectTypeJSON =
    """
    {
        "kind": "INPUT_OBJECT",
        "name": "ThingInput",
        "description": "input object for a thing",
        "inputFields": [
            {
                "name": "grams",
                "description": null,
                "type": {
                    "kind": "NON_NULL",
                    "name": null,
                    "ofType": {
                        "kind": "SCALAR",
                        "name": "Float",
                        "ofType": null
                    }
                },
                "defaultValue": "0"
            }
        ]
    }
    """


directiveJSON =
    """
    {
        "name": "include",
        "description": "Directs the executor to include this field or fragment only when the `if` argument is true.",
        "locations": [
            "FIELD",
            "FRAGMENT_SPREAD",
            "INLINE_FRAGMENT"
        ],
        "args": [
            {
            "name": "if",
            "description": "Included when true.",
            "type": {
                "kind": "NON_NULL",
                "name": null,
                "ofType": {
                    "kind": "SCALAR",
                    "name": "Boolean"
                }
            },
            "defaultValue": null
            }
        ]
    }
    """


almostValidSchemaJSON =
    """
    {
        "queryType": {
            "kind": "OBJECT",
            "name": "Thing",
            "ofType": null
        },
        "mutationType": null,
        "subscriptionType": null,
        "types": [
    """
        ++ String.join ",\n"
            [ scalarTypeJSON
            , objectTypeJSON
            , unionTypeJSON
            , interfaceTypeJSON
            , enumTypeJSON
            , inputObjectTypeJSON
            ]
        ++ """
        ],
        "directives": [
    """
        ++ directiveJSON
        ++ """
        ]
    }
    """


tests : List Test.Test
tests =
    [ test "scalarTypeDecoder" <|
        \() ->
            scalarTypeJSON
                |> Decode.decodeString GraphQL.Schema.Decode.scalarTypeDecoder
                |> Expect.equal
                    (Ok
                        (Schema.ScalarType
                            { name = "Boolean"
                            , description = Just "The `Boolean` scalar type represents `true` or `false`."
                            }
                        )
                    )
    , test "objectTypeDecoder" <|
        \() ->
            objectTypeJSON
                |> Decode.decodeString GraphQL.Schema.Decode.objectTypeDecoder
                |> Expect.equal
                    (Ok
                        (Schema.ObjectType
                            { name = "Thing"
                            , description = Just "A test thing."
                            , fields =
                                [ { name = "mass"
                                  , description = Just "mass of the thing"
                                  , args =
                                        [ { name = "inPounds"
                                          , description = Just "get the mass in pounds"
                                          , valueType = Schema.Ref "Boolean"
                                          , defaultValue = Just "false"
                                          }
                                        ]
                                  , valueType = Schema.NonNull (Schema.Ref "Float")
                                  , isDeprecated = True
                                  , deprecationReason = Just "no good reason"
                                  }
                                ]
                            , interfaces = [ Schema.Ref "Massive" ]
                            }
                        )
                    )
    , test "unionTypeDecoder" <|
        \() ->
            unionTypeJSON
                |> Decode.decodeString GraphQL.Schema.Decode.unionTypeDecoder
                |> Expect.equal
                    (Ok
                        (Schema.UnionType
                            { name = "OnlyThing"
                            , description = Just "A union of Thing and nothing else."
                            , possibleTypes = [ Schema.Ref "Thing" ]
                            }
                        )
                    )
    , test "interfaceTypeDecoder" <|
        \() ->
            interfaceTypeJSON
                |> Decode.decodeString GraphQL.Schema.Decode.interfaceTypeDecoder
                |> Expect.equal
                    (Ok
                        (Schema.InterfaceType
                            { name = "HasMass"
                            , description = Just "has a mass"
                            , fields =
                                [ { name = "mass"
                                  , description = Just "mass of the thing"
                                  , args =
                                        [ { name = "inPounds"
                                          , description = Just "get the mass in pounds"
                                          , valueType = Schema.Ref "Boolean"
                                          , defaultValue = Just "false"
                                          }
                                        ]
                                  , valueType = Schema.NonNull (Schema.Ref "Float")
                                  , isDeprecated = True
                                  , deprecationReason = Just "no good reason"
                                  }
                                ]
                            , possibleTypes = [ Schema.Ref "Thing" ]
                            }
                        )
                    )
    , test "enumTypeDecoder" <|
        \() ->
            enumTypeJSON
                |> Decode.decodeString GraphQL.Schema.Decode.enumTypeDecoder
                |> Expect.equal
                    (Ok
                        (Schema.EnumType
                            { name = "UnitSystem"
                            , description = Just "a system of units"
                            , enumValues =
                                [ { name = "METRIC"
                                  , description = Just "a very popular unit system"
                                  , isDeprecated = False
                                  , deprecationReason = Nothing
                                  }
                                , { name = "IMPERIAL"
                                  , description = Nothing
                                  , isDeprecated = True
                                  , deprecationReason = Just "it's a bit weird"
                                  }
                                ]
                            }
                        )
                    )
    , test "inputObjectTypeDecoder" <|
        \() ->
            inputObjectTypeJSON
                |> Decode.decodeString GraphQL.Schema.Decode.inputObjectTypeDecoder
                |> Expect.equal
                    (Ok
                        (Schema.InputObjectType
                            { name = "ThingInput"
                            , description = Just "input object for a thing"
                            , inputFields =
                                [ { name = "grams"
                                  , description = Nothing
                                  , valueType = Schema.NonNull (Schema.Ref "Float")
                                  , defaultValue = Just "0"
                                  }
                                ]
                            }
                        )
                    )
    , test "directiveDecoder" <|
        \() ->
            directiveJSON
                |> Decode.decodeString GraphQL.Schema.Decode.directiveDecoder
                |> Expect.equal
                    (Ok
                        { name = "include"
                        , description = Just "Directs the executor to include this field or fragment only when the `if` argument is true."
                        , locations =
                            [ Schema.FieldLocation
                            , Schema.FragmentSpreadLocation
                            , Schema.InlineFragmentLocation
                            ]
                        , args =
                            [ { name = "if"
                              , description = Just "Included when true."
                              , valueType = Schema.NonNull (Schema.Ref "Boolean")
                              , defaultValue = Nothing
                              }
                            ]
                        }
                    )
    , test "schemaDecoder" <|
        \() ->
            let
                decodeResult =
                    almostValidSchemaJSON
                        |> Decode.decodeString GraphQL.Schema.Decode.schemaDecoder
            in
                case decodeResult of
                    Ok schema ->
                        let
                            summary =
                                { queryType = schema.queryType
                                , mutationType = schema.mutationType
                                , subscriptionType = schema.subscriptionType
                                , typeNames = Dict.keys schema.types
                                , directiveNames = List.map .name schema.directives
                                }

                            expected =
                                { queryType = "Thing"
                                , mutationType = Nothing
                                , subscriptionType = Nothing
                                , typeNames = [ "Boolean", "HasMass", "OnlyThing", "Thing", "ThingInput", "UnitSystem" ]
                                , directiveNames = [ "include" ]
                                }
                        in
                            Expect.equal expected summary

                    Err err ->
                        Expect.fail (Decode.errorToString err)
    ]


all : Test
all =
    describe "GraphQL.Schema.Decode" tests
