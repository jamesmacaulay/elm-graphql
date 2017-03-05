module GraphQL.Response
    exposing
        ( RequestError
        , DocumentLocation
        , decoder
        )

import Json.Decode as Decode exposing (Decoder)


type alias RequestError =
    { message : String
    , locations : List DocumentLocation
    }


type alias DocumentLocation =
    { line : Int
    , column : Int
    }


decoder : Decoder result -> Decoder (Result (List RequestError) result)
decoder successDecoder =
    Decode.oneOf
        [ Decode.field "data"
            (Decode.map Ok successDecoder)
        , Decode.field "errors"
            (Decode.map Err (Decode.list errorDecoder))
        ]


errorDecoder : Decoder RequestError
errorDecoder =
    Decode.map2 RequestError
        (Decode.field "message" Decode.string)
        (Decode.field "locations" (Decode.list documentLocationDecoder))


documentLocationDecoder : Decoder DocumentLocation
documentLocationDecoder =
    Decode.map2 DocumentLocation
        (Decode.field "line" Decode.int)
        (Decode.field "column" Decode.int)
