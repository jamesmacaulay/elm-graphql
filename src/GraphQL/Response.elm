module GraphQL.Response
    exposing
        ( Error
        , DocumentLocation
        , decoder
        , successDecoder
        , errorsDecoder
        )

import Json.Decode as Decode exposing (Decoder)


type alias Error =
    { message : String
    , locations : List DocumentLocation
    }


type alias DocumentLocation =
    { line : Int
    , column : Int
    }


decoder : Decoder result -> Decoder (Result (List Error) result)
decoder d =
    Decode.oneOf
        [ Decode.map Ok (successDecoder d)
        , Decode.map Err errorsDecoder
        ]


successDecoder : Decoder result -> Decoder result
successDecoder decoder =
    Decode.field "data" decoder


errorsDecoder : Decoder (List Error)
errorsDecoder =
    Decode.field "errors"
        (Decode.list
            (Decode.map2 Error
                (Decode.field "message" Decode.string)
                (Decode.oneOf
                    [ Decode.field "locations" (Decode.list documentLocationDecoder)
                    , Decode.succeed []
                    ]
                )
            )
        )


documentLocationDecoder : Decoder DocumentLocation
documentLocationDecoder =
    Decode.map2 DocumentLocation
        (Decode.field "line" Decode.int)
        (Decode.field "column" Decode.int)
