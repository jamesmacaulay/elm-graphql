module Tests exposing (..)

import Test exposing (..)
import GraphQL.Request.BuilderTests
import GraphQL.Client.HttpTests
import GraphQL.Schema.DecodeTests


all : Test
all =
    describe "All"
        [ GraphQL.Request.BuilderTests.all
        , GraphQL.Client.HttpTests.all
        , GraphQL.Schema.DecodeTests.all
        ]
