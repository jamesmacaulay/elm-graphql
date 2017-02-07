module GraphQL.Query.BuilderTests exposing (..)

import Test exposing (..)
import Expect
import GraphQL.Query.Builder as Q
import GraphQL.Query.Builder.Structure as S
import Json.Decode as Decode
import String


testSpecSuccess :
    String
    -> Q.Spec a
    -> S.Spec
    -> Test.Test
testSpecSuccess expr decodableSpec expectedSpec =
    test ("Spec success for " ++ expr) <|
        \() ->
            decodableSpec
                |> Q.getNode
                |> Expect.equal (S.Builder [] expectedSpec)


testSpecErrors :
    String
    -> Q.Spec a
    -> List S.BuilderError
    -> Test.Test
testSpecErrors expr decodableSpec expectedErrors =
    test ("Spec errors for " ++ expr) <|
        \() ->
            let
                (S.Builder errors _) =
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
        S.IntSpec
    , testDecoder "int"
        Q.int
        "1234"
        1234
    , testSpecSuccess "float"
        Q.float
        S.FloatSpec
    , testDecoder "float"
        Q.float
        "12.34"
        12.34
    , testSpecSuccess "string"
        Q.string
        S.StringSpec
    , testDecoder "string"
        Q.string
        "\"hello\""
        "hello"
    , testSpecSuccess "bool"
        Q.bool
        S.BooleanSpec
    , testDecoder "bool"
        Q.bool
        "true"
        True
    , testSpecSuccess "(nullable int)"
        (Q.nullable Q.int)
        (S.NullableSpec S.IntSpec)
    , testDecoder "(nullable int) decoding int"
        (Q.nullable Q.int)
        "1"
        (Just 1)
    , testDecoder "(nullable int) decoding null"
        (Q.nullable Q.int)
        "null"
        Nothing
    , testSpecSuccess "(list int)"
        (Q.list Q.int)
        (S.ListSpec S.IntSpec)
    , testDecoder "(list int)"
        (Q.list Q.int)
        "[1, 2, 3]"
        [ 1, 2, 3 ]
    , testSpecSuccess "(produce (,) |> withField ...)"
        (Q.produce (,)
            |> Q.withField "name" [] Q.string
            |> Q.withField "number" [] Q.int
        )
        (S.ObjectSpec
            [ S.FieldSelection
                { name = "name"
                , spec = S.StringSpec
                , fieldAlias = Nothing
                , args = []
                , directives = []
                }
            , S.FieldSelection
                { name = "number"
                , spec = S.IntSpec
                , fieldAlias = Nothing
                , args = []
                , directives = []
                }
            ]
        )
    , testDecoder "(produce (,) |> withField ...)"
        (Q.produce (,)
            |> Q.withField "name" [ Q.fieldAlias "nameAlias" ] Q.string
            |> Q.withField "number" [] Q.int
        )
        "{\"nameAlias\":\"Alice\",\"number\":33}"
        ( "Alice", 33 )
    , testSpecSuccess "(produce (,) |> andMap ...)"
        (Q.produce (,)
            |> Q.andMap (Q.map String.reverse Q.string)
            |> Q.andMap (Q.map String.length Q.string)
        )
        S.StringSpec
    , testDecoder "(produce (,) |> andMap ...)"
        (Q.produce (,)
            |> Q.andMap (Q.map String.reverse Q.string)
            |> Q.andMap (Q.map String.length Q.string)
        )
        "\"foo\""
        ( "oof", 3 )
    , testSpecErrors "(produce (,,) ... trying to intersect with int and bool"
        (Q.produce (,,)
            |> Q.withField "name" [] Q.string
            |> Q.andMap Q.int
            |> Q.andMap Q.bool
        )
        [ S.InvalidIntersection
            (S.ObjectSpec
                ([ S.FieldSelection
                    { name = "name"
                    , spec = S.StringSpec
                    , fieldAlias = Nothing
                    , args = []
                    , directives = []
                    }
                 ]
                )
            )
            S.IntSpec
        , S.InvalidIntersection
            (S.ObjectSpec
                ([ S.FieldSelection
                    { name = "name"
                    , spec = S.StringSpec
                    , fieldAlias = Nothing
                    , args = []
                    , directives = []
                    }
                 ]
                )
            )
            S.BooleanSpec
        ]
    ]


all : Test.Test
all =
    describe "GraphQL.Query" tests
