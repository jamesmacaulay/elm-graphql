module GraphQL.Client.HttpTests exposing (..)

import Expect
import GraphQL.Client.Http
import GraphQL.Client.Http.Util as Util
import Json.Encode
import Test exposing (..)


minimalVars : Json.Encode.Value
minimalVars =
    Json.Encode.object [ ( "bar", Json.Encode.int 0 ) ]


tests : List Test.Test
tests =
    [ test "postBodyJson with no variables" <|
        \() ->
            Util.postBodyJson "{ foo }" Nothing
                |> Json.Encode.encode 0
                |> Expect.equal """{"query":"{ foo }"}"""
    , test "postBodyJson with variables" <|
        \() ->
            Util.postBodyJson "{ foo }" (Just minimalVars)
                |> Json.Encode.encode 0
                |> Expect.equal """{"query":"{ foo }","variables":{"bar":0}}"""
    , test "parameterizedUrl with no variables" <|
        \() ->
            Util.parameterizedUrl "/" "{ foo }" Nothing
                |> Expect.equal "/?query=%7B%20foo%20%7D"
    , test "parameterizedUrl with variables" <|
        \() ->
            Util.parameterizedUrl "/" "{ foo }" (Just minimalVars)
                |> Expect.equal "/?query=%7B%20foo%20%7D&variables=%7B%22bar%22%3A0%7D"
    , test "parameterizedUrl on a url with a query string" <|
        \() ->
            Util.parameterizedUrl "/?baz=false" "{ foo }" (Just minimalVars)
                |> Expect.equal "/?baz=false&query=%7B%20foo%20%7D&variables=%7B%22bar%22%3A0%7D"
    ]


all : Test.Test
all =
    describe "GraphQL.Client.Http" tests
