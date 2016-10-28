module GraphQL.QueryTests exposing (..)

import Test exposing (..)
import Expect
import GraphQL.Query as Q
import Json.Decode as Decode


-- specTest : String -> Q.Decodable Q.ValueSpec a -> Test.Test
-- specTest expr decodableValueSpec =
--     test (expr ++ "'s spec is )


testValueSpec :
    String
    -> Q.Decodable Q.ValueSpec a
    -> Q.ValueSpec
    -> Test.Test
testValueSpec expr decodableValueSpec expectedValueSpec =
    test (expr ++ "'s ValueSpec")
        <| \() ->
            decodableValueSpec
                |> Q.getNode
                |> Expect.equal expectedValueSpec


testDecoder :
    String
    -> Q.Decodable Q.ValueSpec a
    -> String
    -> a
    -> Test.Test
testDecoder expr decodableValueSpec testJSON expectedResult =
    test (expr ++ "'s Decoder")
        <| \() ->
            decodableValueSpec
                |> Q.getDecoder
                |> flip Decode.decodeString testJSON
                |> Expect.equal (Ok expectedResult)


tests : List Test.Test
tests =
    [ testValueSpec "int"
        Q.int
        Q.IntSpec
    , testDecoder "int"
        Q.int
        "1234"
        1234
    , testValueSpec "float"
        Q.float
        Q.FloatSpec
    , testDecoder "float"
        Q.float
        "12.34"
        12.34
    , testValueSpec "string"
        Q.string
        Q.StringSpec
    , testDecoder "string"
        Q.string
        "\"hello\""
        "hello"
    , testValueSpec "bool"
        Q.bool
        Q.BooleanSpec
    , testDecoder "bool"
        Q.bool
        "true"
        True
    , testValueSpec "(list int)"
        (Q.list Q.int)
        (Q.ListSpec Q.IntSpec)
    , testDecoder "(list int)"
        (Q.list Q.int)
        "[1, 2, 3]"
        [ 1, 2, 3 ]
    , testValueSpec "(construct (,) ...)"
        (Q.construct (,)
            |> Q.withField "name" [] Q.string
            |> Q.withField "number" [] Q.int
            |> Q.fromObject
        )
        (Q.ObjectSpec
            (Q.SelectionSet
                [ Q.FieldSelection
                    (Q.Field
                        { name = "name"
                        , valueSpec = Q.StringSpec
                        , fieldAlias = Nothing
                        , args = []
                        , directives = []
                        }
                    )
                , Q.FieldSelection
                    (Q.Field
                        { name = "number"
                        , valueSpec = Q.IntSpec
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
