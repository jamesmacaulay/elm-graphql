module GraphQL.Query.EncodeTests exposing (..)

import Test exposing (..)
import Expect
import GraphQL.Query as Q
import GraphQL.Query.Encode as QE


tests : List Test.Test
tests =
    [ test "encoding a very simple selection set"
        <| \() ->
            Q.object (,)
                |> Q.withField "name" [] Q.string
                |> Q.withField "number" [] Q.int
                |> Q.query
                |> Q.getNode
                |> QE.encodeQuery
                |> Expect.equal """{
  name
  number
}"""
    ]


all : Test
all =
    describe "GraphQL.Query.Encode" tests
