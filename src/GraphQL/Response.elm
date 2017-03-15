module GraphQL.Response
    exposing
        ( RequestError
        , DocumentLocation
        , errorsDecoder
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


errorsDecoder : Decoder (List RequestError)
errorsDecoder =
    Decode.list
        (Decode.map2 RequestError
            (Decode.field "message" Decode.string)
            (Decode.oneOf
                [ Decode.field "locations" (Decode.list documentLocationDecoder)
                , Decode.succeed []
                ]
            )
        )


documentLocationDecoder : Decoder DocumentLocation
documentLocationDecoder =
    Decode.map2 DocumentLocation
        (Decode.field "line" Decode.int)
        (Decode.field "column" Decode.int)
