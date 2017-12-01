module GraphQL.Request.Document.AST.ValidateTests exposing (..)

import Test exposing (..)
import Expect
import GraphQL.Request.Document.AST.Validate exposing (..)
import GraphQL.Request.Document.AST as AST
import GraphQL.Schema as Schema exposing (Schema)
import Dict exposing (Dict)


testOperationNameUniqueness : Test
testOperationNameUniqueness =
    describe "validateOperationNameUniqueness"
        [ test "should return True for unique operation names" <|
            (\_ ->
                let
                    document =
                        AST.Document
                            [ { baseOperationDefinitionInfo | name = Just "operation1" } |> AST.OperationDefinition
                            , { baseOperationDefinitionInfo | name = Just "operation2" } |> AST.OperationDefinition
                            ]
                in
                    case validateOperationNameUniqueness document of
                        Just error ->
                            Expect.fail "unique names were incorrectly identified as non-unique"

                        Nothing ->
                            Expect.pass
            )
        , test "should return False for duplicate operation names" <|
            (\_ ->
                let
                    document =
                        AST.Document
                            [ { baseOperationDefinitionInfo | name = Just "operation1" } |> AST.OperationDefinition
                            , { baseOperationDefinitionInfo | name = Just "operation1" } |> AST.OperationDefinition
                            ]
                in
                    case validateOperationNameUniqueness document of
                        Just error ->
                            Expect.pass

                        Nothing ->
                            Expect.fail "Duplicate names were not caught in validation"
            )
        ]


testLongAnonymousOperation : Test
testLongAnonymousOperation =
    describe "validateLoneAnonymousOperation"
        [ test "should allow lone anonymous operations" <|
            (\_ ->
                let
                    document =
                        AST.Document
                            [ { baseOperationDefinitionInfo | name = Just "test" } |> AST.OperationDefinition
                            ]
                in
                    case validateLoneAnonymousOperation document of
                        Just error ->
                            Expect.fail "lone anonymous operation was incorrectly failed during validation"

                        Nothing ->
                            Expect.pass
            )
        , test "should allow lone query shorthand operations" <|
            (\_ ->
                let
                    document =
                        AST.Document
                            [ AST.QueryShorthand (AST.SelectionSet [])
                            ]
                in
                    case validateLoneAnonymousOperation document of
                        Just error ->
                            Expect.fail "lone anonymous operation was incorrectly failed during validation"

                        Nothing ->
                            Expect.pass
            )
        , test "should allow multiple named operations" <|
            (\_ ->
                let
                    document =
                        AST.Document
                            [ { baseOperationDefinitionInfo | name = Just "operation1" } |> AST.OperationDefinition
                            , { baseOperationDefinitionInfo | name = Just "operation2" } |> AST.OperationDefinition
                            ]
                in
                    case validateLoneAnonymousOperation document of
                        Just error ->
                            Expect.fail "multiple named operations were incorrectly failed during validation"

                        Nothing ->
                            Expect.pass
            )
        , test "should disallow mixed named and anonymous operations" <|
            (\_ ->
                let
                    document =
                        AST.Document
                            [ { baseOperationDefinitionInfo | name = Just "operation1" } |> AST.OperationDefinition
                            , { baseOperationDefinitionInfo | name = Nothing } |> AST.OperationDefinition
                            ]
                in
                    case validateLoneAnonymousOperation document of
                        Just error ->
                            Expect.pass

                        Nothing ->
                            Expect.fail "mixed anonymous/named operations were incorrectly passed during validation"
            )
        , test "should disallow mixed named and shorthand operations" <|
            (\_ ->
                let
                    document =
                        AST.Document
                            [ { baseOperationDefinitionInfo | name = Just "operation1" } |> AST.OperationDefinition
                            , AST.QueryShorthand (AST.SelectionSet [])
                            ]
                in
                    case validateLoneAnonymousOperation document of
                        Just error ->
                            Expect.pass

                        Nothing ->
                            Expect.fail "mixed anonymous/named operations were incorrectly passed during validation"
            )
        ]


testFieldSelectionMerging : Test
testFieldSelectionMerging =
    describe "validateFieldSelectionMerging"
        [ test "should merge normal fields" <|
            (\_ ->
                let
                    makeOperation =
                        { baseOperationDefinitionInfo
                            | selectionSet =
                                AST.SelectionSet
                                    [ { baseFieldInfo | name = "duplicateField" } |> AST.Field
                                    , { baseFieldInfo | name = "duplicateField" } |> AST.Field
                                    ]
                        }
                            |> AST.OperationDefinition

                    document =
                        AST.Document
                            [ makeOperation ]
                in
                    case validateFieldSelectionMerging baseSchema document of
                        Just error ->
                            Expect.fail "Failed to merge normal fields"

                        Nothing ->
                            Expect.pass
            )
        , test "should merge aliased fields with same alias" <|
            (\_ ->
                let
                    field =
                        { baseFieldInfo | alias = Just "duplicateAlias", name = "duplicateName" } |> AST.Field

                    makeOperation =
                        { baseOperationDefinitionInfo
                            | selectionSet =
                                AST.SelectionSet
                                    [ field
                                    , field
                                    ]
                        }
                            |> AST.OperationDefinition

                    document =
                        AST.Document
                            [ makeOperation ]
                in
                    case validateFieldSelectionMerging baseSchema document of
                        Just error ->
                            Expect.fail "Failed to merge fields with same alias"

                        Nothing ->
                            Expect.pass
            )
        , test "should fail to merge aliased fields with different alias" <|
            (\_ ->
                let
                    makeOperation =
                        { baseOperationDefinitionInfo
                            | selectionSet =
                                AST.SelectionSet
                                    [ { baseFieldInfo | alias = Just "oneAlias", name = "duplicateName" } |> AST.Field
                                    , { baseFieldInfo | alias = Just "anotherAlias", name = "duplicateName" } |> AST.Field
                                    ]
                        }
                            |> AST.OperationDefinition

                    document =
                        AST.Document
                            [ makeOperation ]
                in
                    case validateFieldSelectionMerging baseSchema document of
                        Just error ->
                            Expect.pass

                        Nothing ->
                            Expect.fail "Incorrectly merged fields with same name but different aliases"
            )
        , test "should fail to merge aliased field with non-aliased field" <|
            (\_ ->
                let
                    makeOperation =
                        { baseOperationDefinitionInfo
                            | selectionSet =
                                AST.SelectionSet
                                    [ { baseFieldInfo | alias = Just "anAlias", name = "duplicateName" } |> AST.Field
                                    , { baseFieldInfo | name = "duplicateName" } |> AST.Field
                                    ]
                        }
                            |> AST.OperationDefinition

                    document =
                        AST.Document
                            [ makeOperation ]
                in
                    case validateFieldSelectionMerging baseSchema document of
                        Just error ->
                            Expect.pass

                        Nothing ->
                            Expect.fail "Incorrectly merged aliased field with non-aliased field"
            )
        , test "should merge fields with identical arguments" <|
            (\_ ->
                let
                    field =
                        { baseFieldInfo | name = "duplicateName", arguments = fieldArguments } |> AST.Field

                    fieldArguments =
                        [ ( "argument1", AST.NullValue )
                        , ( "argument2", AST.IntValue 3 )
                        ]

                    makeOperation =
                        { baseOperationDefinitionInfo
                            | selectionSet =
                                AST.SelectionSet
                                    [ field
                                    , field
                                    ]
                        }
                            |> AST.OperationDefinition

                    document =
                        AST.Document
                            [ makeOperation ]
                in
                    case validateFieldSelectionMerging baseSchema document of
                        Just error ->
                            Expect.fail "Failed to merge fields with identical arguments"

                        Nothing ->
                            Expect.pass
            )
        , test "should fail to merge fields with different arguments" <|
            (\_ ->
                let
                    -- Argument values are different
                    fieldArgumentsA =
                        [ ( "argument1", AST.NullValue )
                        , ( "argument2", AST.IntValue 3 )
                        ]

                    fieldArgumentsB =
                        [ ( "argument1", AST.FloatValue 3.4 )
                        , ( "argument2", AST.IntValue 4 )
                        ]

                    makeOperation =
                        { baseOperationDefinitionInfo
                            | selectionSet =
                                AST.SelectionSet
                                    [ { baseFieldInfo | name = "duplicateName", arguments = fieldArgumentsA } |> AST.Field
                                    , { baseFieldInfo | name = "duplicateName", arguments = fieldArgumentsB } |> AST.Field
                                    ]
                        }
                            |> AST.OperationDefinition

                    document =
                        AST.Document
                            [ makeOperation ]
                in
                    case validateFieldSelectionMerging baseSchema document of
                        Just error ->
                            Expect.fail "Failed to merge fields with identical arguments"

                        Nothing ->
                            Expect.pass
            )
        , todo "should merge fields with identical constant values"
        , todo "should fail to merge fields with different constant values"
        , todo "should fail to merge field with argument parameter and field with constant parameter"
        , todo "should fail to merge field with arguments with field with no arguments"
        , todo "should merge differing fields on different inline fragments"
        , todo "should fail to merge fields with differing types on different inline fragments"
        ]



-- Test Utils


baseSchema : Schema
baseSchema =
    { queryType = ""
    , mutationType = Nothing
    , subscriptionType = Nothing
    , types = Dict.fromList []
    , directives = []
    }


baseFieldInfo =
    { alias = Nothing
    , name = "field"
    , arguments = []
    , directives = []
    , selectionSet = AST.SelectionSet []
    }


baseOperationDefinitionInfo : AST.OperationDefinitionInfo
baseOperationDefinitionInfo =
    { operationType = AST.Query
    , name = Just "operation"
    , variableDefinitions = []
    , directives = []
    , selectionSet = AST.SelectionSet []
    }
