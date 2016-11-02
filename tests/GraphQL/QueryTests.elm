module GraphQL.QueryTests exposing (..)

import Test exposing (..)
import Expect
import GraphQL.Query as Q
import Json.Decode as Decode


testSpec :
    String
    -> Q.Decodable Q.Spec a
    -> Q.Spec
    -> Test.Test
testSpec expr decodableSpec expectedSpec =
    test ("Spec for " ++ expr)
        <| \() ->
            decodableSpec
                |> Q.getNode
                |> Expect.equal expectedSpec


testDecoder :
    String
    -> Q.Decodable Q.Spec a
    -> String
    -> a
    -> Test.Test
testDecoder expr decodableSpec testJSON expectedResult =
    test ("Decoder for " ++ expr)
        <| \() ->
            decodableSpec
                |> Q.getDecoder
                |> flip Decode.decodeString testJSON
                |> Expect.equal (Ok expectedResult)


tests : List Test.Test
tests =
    [ testSpec "int"
        Q.int
        Q.IntSpec
    , testDecoder "int"
        Q.int
        "1234"
        1234
    , testSpec "float"
        Q.float
        Q.FloatSpec
    , testDecoder "float"
        Q.float
        "12.34"
        12.34
    , testSpec "string"
        Q.string
        Q.StringSpec
    , testDecoder "string"
        Q.string
        "\"hello\""
        "hello"
    , testSpec "bool"
        Q.bool
        Q.BooleanSpec
    , testDecoder "bool"
        Q.bool
        "true"
        True
    , testSpec "(list int)"
        (Q.list Q.int)
        (Q.ListSpec Q.IntSpec)
    , testDecoder "(list int)"
        (Q.list Q.int)
        "[1, 2, 3]"
        [ 1, 2, 3 ]
    , testSpec "(object (,) |> withField ...)"
        (Q.object (,)
            |> Q.withField "name" [] Q.string
            |> Q.withField "number" [] Q.int
        )
        (Q.ObjectSpec
            (Q.SelectionSet
                [ Q.FieldSelection
                    (Q.Field
                        { name = "name"
                        , spec = Q.StringSpec
                        , fieldAlias = Nothing
                        , args = []
                        , directives = []
                        }
                    )
                , Q.FieldSelection
                    (Q.Field
                        { name = "number"
                        , spec = Q.IntSpec
                        , fieldAlias = Nothing
                        , args = []
                        , directives = []
                        }
                    )
                ]
            )
        )
    ]


all : Test
all =
    describe "GraphQL.Query" tests
