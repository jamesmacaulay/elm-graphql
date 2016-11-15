module GraphQL.QueryTests exposing (..)

import Test exposing (..)
import Expect
import GraphQL.Query as Q
import Json.Decode as Decode
import String


testSpecSuccess :
    String
    -> Q.Spec a
    -> Q.SpecStructure
    -> Test.Test
testSpecSuccess expr decodableSpec expectedSpec =
    test ("Spec success for " ++ expr) <|
        \() ->
            decodableSpec
                |> Q.getNode
                |> Expect.equal (Q.Builder [] expectedSpec)


testSpecErrors :
    String
    -> Q.Spec a
    -> List Q.BuilderError
    -> Test.Test
testSpecErrors expr decodableSpec expectedErrors =
    test ("Spec errors for " ++ expr) <|
        \() ->
            let
                (Q.Builder errors _) =
                    Q.getNode decodableSpec
            in
                Expect.equal expectedErrors errors


testDecoder :
    String
    -> Q.Spec a
    -> String
    -> a
    -> Test.Test
testDecoder expr decodableSpec testJSON expectedResult =
    test ("Decoder for " ++ expr) <|
        \() ->
            decodableSpec
                |> Q.getDecoder
                |> flip Decode.decodeString testJSON
                |> Expect.equal (Ok expectedResult)


tests : List Test.Test
tests =
    [ testSpecSuccess "int"
        Q.int
        Q.IntSpec
    , testDecoder "int"
        Q.int
        "1234"
        1234
    , testSpecSuccess "float"
        Q.float
        Q.FloatSpec
    , testDecoder "float"
        Q.float
        "12.34"
        12.34
    , testSpecSuccess "string"
        Q.string
        Q.StringSpec
    , testDecoder "string"
        Q.string
        "\"hello\""
        "hello"
    , testSpecSuccess "bool"
        Q.bool
        Q.BooleanSpec
    , testDecoder "bool"
        Q.bool
        "true"
        True
    , testSpecSuccess "(list int)"
        (Q.list Q.int)
        (Q.ListSpec Q.IntSpec)
    , testDecoder "(list int)"
        (Q.list Q.int)
        "[1, 2, 3]"
        [ 1, 2, 3 ]
    , testSpecSuccess "(object (,) |> withField ...)"
        (Q.object (,)
            |> Q.withField "name" [] Q.string
            |> Q.withField "number" [] Q.int
        )
        (Q.ObjectSpec
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
    , testDecoder "(object (,) |> withField ...)"
        (Q.object (,)
            |> Q.withField "name" [] Q.string
            |> Q.withField "number" [] Q.int
        )
        "{\"name\":\"Alice\",\"number\":33}"
        ( "Alice", 33 )
    , testSpecSuccess "(construct (,) |> andMap ...)"
        (Q.construct (,)
            |> Q.andMap (Q.map String.reverse Q.string)
            |> Q.andMap (Q.map String.length Q.string)
        )
        Q.StringSpec
    , testDecoder "(construct (,) |> andMap ...)"
        (Q.construct (,)
            |> Q.andMap (Q.map String.reverse Q.string)
            |> Q.andMap (Q.map String.length Q.string)
        )
        "\"foo\""
        ( "oof", 3 )
    , testSpecErrors "(object (,,) ... trying to intersect with int and bool"
        (Q.object (,,)
            |> Q.withField "name" [] Q.string
            |> Q.andMap Q.int
            |> Q.andMap Q.bool
        )
        [ Q.InvalidIntersection
            (Q.ObjectSpec
                ([ Q.FieldSelection
                    (Q.Field
                        { name = "name"
                        , spec = Q.StringSpec
                        , fieldAlias = Nothing
                        , args = []
                        , directives = []
                        }
                    )
                 ]
                )
            )
            Q.IntSpec
        , Q.InvalidIntersection
            (Q.ObjectSpec
                ([ Q.FieldSelection
                    (Q.Field
                        { name = "name"
                        , spec = Q.StringSpec
                        , fieldAlias = Nothing
                        , args = []
                        , directives = []
                        }
                    )
                 ]
                )
            )
            Q.BooleanSpec
        ]
    ]


all : Test
all =
    describe "GraphQL.Query" tests
