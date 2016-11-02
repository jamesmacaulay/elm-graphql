module GraphQL.Query.EncodeTests exposing (..)

import Test exposing (..)
import Expect
import GraphQL.Query as Q
import GraphQL.Query.Arg as Arg
import GraphQL.Query.Encode as QE


tests : List Test.Test
tests =
    [ test "encoding a very simple query"
        <| \() ->
            Q.object (,)
                |> Q.withField "name" [] Q.string
                |> Q.withField "number" [] Q.int
                |> Q.query []
                |> Q.getNode
                |> QE.encodeQuery
                |> Expect.equal """{
  name
  number
}"""
    , test "encoding a more complex query"
        <| \() ->
            Q.object identity
                |> Q.withField "user"
                    [ Q.fieldArgs [ ( "id", Arg.string "123" ) ] ]
                    (Q.object (,)
                        |> Q.withField "name" [] Q.string
                        |> Q.withField "photos"
                            [ Q.fieldArgs [ ( "first", Arg.int 10 ) ] ]
                            (Q.list
                                (Q.object (,)
                                    |> Q.withField "url" [] Q.string
                                    |> Q.withField "caption" [] Q.string
                                )
                            )
                    )
                |> Q.query [ Q.queryName "userQuery" ]
                |> Q.getNode
                |> QE.encodeQuery
                |> Expect.equal """query userQuery {
  user(id: "123") {
    name
    photos(first: 10) {
      url
      caption
    }
  }
}"""
    ]


all : Test
all =
    describe "GraphQL.Query.Encode" tests
