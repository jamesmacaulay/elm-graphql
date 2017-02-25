module GraphQL.Query.BuilderTests exposing (..)

import Test exposing (..)
import Expect
import GraphQL.Query.Builder as Q
import GraphQL.Query.Builder.Structure as S
import Json.Decode as Decode
import String


testSpecSuccess :
    String
    -> Q.Spec a b
    -> S.SpecD
    -> Test.Test
testSpecSuccess expr decodableSpec expectedSpec =
    test ("Spec success for " ++ expr) <|
        \() ->
            decodableSpec
                |> Q.getStructure
                |> S.getSpecFromStructure
                |> Expect.equal expectedSpec


testDecoder :
    String
    -> Q.Spec a b
    -> String
    -> b
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
        S.IntSpecD
    , testDecoder "int"
        Q.int
        "1234"
        1234
    , testSpecSuccess "float"
        Q.float
        S.FloatSpecD
    , testDecoder "float"
        Q.float
        "12.34"
        12.34
    , testSpecSuccess "string"
        Q.string
        S.StringSpecD
    , testDecoder "string"
        Q.string
        "\"hello\""
        "hello"
    , testSpecSuccess "bool"
        Q.bool
        S.BooleanSpecD
    , testDecoder "bool"
        Q.bool
        "true"
        True
    , testSpecSuccess "(nullable int)"
        (Q.nullable Q.int)
        (S.NullableSpecD S.IntSpecD)
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
        (S.ListSpecD S.IntSpecD)
    , testDecoder "(list int)"
        (Q.list Q.int)
        "[1, 2, 3]"
        [ 1, 2, 3 ]
    , testSpecSuccess "(produce (,) |> withField ...)"
        (Q.produce (,)
            |> Q.withField "name" [] Q.string
            |> Q.withField "number" [] Q.int
        )
        (S.ObjectSpecD
            [ S.FieldSelection
                { name = "name"
                , spec = S.StringSpecD
                , fieldAlias = Nothing
                , args = []
                , directives = []
                }
            , S.FieldSelection
                { name = "number"
                , spec = S.IntSpecD
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
        S.StringSpecD
    , testDecoder "(produce (,) |> andMap ...)"
        (Q.produce (,)
            |> Q.andMap (Q.map String.reverse Q.string)
            |> Q.andMap (Q.map String.length Q.string)
        )
        "\"foo\""
        ( "oof", 3 )
    ]


all : Test.Test
all =
    describe "GraphQL.Query" tests
