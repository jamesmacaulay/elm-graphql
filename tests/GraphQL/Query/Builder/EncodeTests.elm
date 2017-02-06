module GraphQL.Query.Builder.EncodeTests exposing (..)

import Test exposing (..)
import Expect
import GraphQL.Query.Builder as Q
import GraphQL.Query.Builder.Arg as Arg
import GraphQL.Query.Builder.Encode as QE


tests : List Test.Test
tests =
    [ test "encoding a very simple query" <|
        \() ->
            Q.object (,)
                |> Q.withField "name" [] Q.string
                |> Q.withField "number" [] Q.int
                |> Q.query []
                |> Q.getNode
                |> QE.encodeQueryBuilder
                |> Expect.equal (Ok """query {
  name
  number
}""")
    , test "encoding a more complex query" <|
        \() ->
            Q.object identity
                |> Q.withField "user"
                    [ Q.fieldArgs [ ( "id", Arg.variable "userId" ) ] ]
                    (Q.object (,)
                        |> Q.withField "name"
                            [ Q.fieldDirective "skip"
                                [ ( "if", Arg.variable "skipName" ) ]
                            ]
                            Q.string
                        |> Q.withField "photos"
                            [ Q.fieldArgs [ ( "first", Arg.int 10 ) ] ]
                            (Q.list
                                (Q.object (,)
                                    |> Q.withField "url" [] Q.string
                                    |> Q.withField "caption" [] Q.string
                                )
                            )
                    )
                |> Q.query
                    [ Q.queryName "userQuery"
                    , Q.queryVariable "userId" "String!"
                    , Q.queryVariableWithDefault "skipName" "Boolean" Arg.true
                    , Q.queryDirective "someQueryDirective" [ ( "foo", Arg.string "bar" ) ]
                    ]
                |> Q.getNode
                |> QE.encodeQueryBuilder
                |> Expect.equal (Ok """query userQuery($userId: String!, $skipName: Boolean = true) @someQueryDirective(foo: "bar") {
  user(id: $userId) {
    name @skip(if: $skipName)
    photos(first: 10) {
      url
      caption
    }
  }
}""")
    ]


all : Test.Test
all =
    describe "GraphQL.Query.Encode" tests
