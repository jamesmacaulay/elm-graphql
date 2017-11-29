module GraphQL.Request.Document.AST.ValidateTests exposing (..)

import Test exposing (..)
import Expect
import GraphQL.Request.Document.AST.Validate exposing (..)
import GraphQL.Request.Document.AST as AST


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



-- Test Utils


baseOperationDefinitionInfo : AST.OperationDefinitionInfo
baseOperationDefinitionInfo =
    { operationType = AST.Query
    , name = Just "operation"
    , variableDefinitions = []
    , directives = []
    , selectionSet = AST.SelectionSet []
    }
