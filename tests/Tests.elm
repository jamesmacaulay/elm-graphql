module Tests exposing (..)

import Test exposing (..)
import GraphQL.Schema.DecodeTests
import GraphQL.Query.BuilderTests
import GraphQL.Query.Builder.EncodeTests
import GraphQL.Client.HttpTests


all : Test
all =
    describe "All"
        [ GraphQL.Schema.DecodeTests.all
        , GraphQL.Query.BuilderTests.all
        , GraphQL.Query.Builder.EncodeTests.all
        , GraphQL.Client.HttpTests.all
        ]
