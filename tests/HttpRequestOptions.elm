module HttpRequestOptions exposing (..)

import Http
import Json.Decode as Decode
import Time exposing (Time)


myGetRequest : Http.Request (List String)
myGetRequest =
    get "/foo"
        (Decode.list Decode.string)
        [ .headers => [ Http.header "foo" "bar" ]
        , .timeout => (10 * Time.second)
        ]


type alias RequestConfig a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect a
    , timeout : Maybe Time
    , withCredentials : Bool
    }


type ConcreteRequestOption a
    = HeadersOption (List Http.Header)
    | BodyOption Http.Body
    | ExpectOption (Http.Expect a)
    | TimeoutOption Time
    | WithCredentialsOption Bool


applyRequestOption :
    ConcreteRequestOption a
    -> RequestConfig a
    -> RequestConfig a
applyRequestOption option config =
    case option of
        HeadersOption headers ->
            { config | headers = headers }

        BodyOption body ->
            { config | body = body }

        ExpectOption expect ->
            { config | expect = expect }

        TimeoutOption timeout ->
            { config | timeout = Just timeout }

        WithCredentialsOption withCredentials ->
            { config | withCredentials = withCredentials }


type alias RequestOptionConstructors a =
    { headers : List Http.Header -> ConcreteRequestOption a
    , body : Http.Body -> ConcreteRequestOption a
    , expect : Http.Expect a -> ConcreteRequestOption a
    , timeout : Time -> ConcreteRequestOption a
    , withCredentials : Bool -> ConcreteRequestOption a
    }


requestOptionConstructors : RequestOptionConstructors a
requestOptionConstructors =
    { headers = HeadersOption
    , body = BodyOption
    , expect = ExpectOption
    , timeout = TimeoutOption
    , withCredentials = WithCredentialsOption
    }


type alias RequestOption a =
    RequestOptionConstructors a -> ConcreteRequestOption a


get : String -> Decode.Decoder a -> List (RequestOption a) -> Http.Request a
get url decoder options =
    options
        |> List.foldl
            ((|>) requestOptionConstructors >> applyRequestOption)
            { method = "GET"
            , headers = []
            , url = url
            , body = Http.emptyBody
            , expect = Http.expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }
        |> Http.request


(=>) : (a -> b -> c) -> b -> a -> c
(=>) =
    flip
