module Main exposing (..)

import Html exposing (Html, div, text)
import GraphQL.Query exposing (..)
import GraphQL.Query.Arg as Arg
import GraphQL.Query.Encode exposing (encodeQueryBuilder)
import Json.Decode
import Json.Encode
import Http
import Task exposing (Task)


type alias FilmSummary =
    { title : Maybe String
    , someCharacterNames : List (Maybe String)
    }


extractConnectionNodes : Spec a -> Spec (List a)
extractConnectionNodes spec =
    (extractField "edges" [] (list (extractField "node" [] spec)))


{-| The definition of `starWarsQuery` builds up a decodable query object that
will later be encoded into the following GraphQL query to send to the server:

{
  film(filmID: 1) {
    title
    characterConnection(first: 3) {
      edges {
        node {
          name
        }
      }
    }
  }
}

The same decodable query value is then also used to decode the response into a
`FilmSummary`.
-}
starWarsQuery : Query FilmSummary
starWarsQuery =
    extractField "film"
        [ fieldArgs [ ( "filmID", Arg.int 1 ) ] ]
        (object FilmSummary
            |> withField "title" [] (maybe string)
            |> withField "characterConnection"
                [ fieldArgs [ ( "first", Arg.int 3 ) ] ]
                (extractConnectionNodes (extractField "name" [] (maybe string)))
        )
        |> query []


type Error
    = HttpError Http.Error
    | InvalidQueryError (List BuilderError)


type alias Model =
    Maybe (Result Error FilmSummary)


type Msg
    = ReceiveQueryResponse (Result Error FilmSummary)


performStarWarsQuery : Query a -> Task Error a
performStarWarsQuery decodableQuery =
    case (decodableQuery |> getNode |> encodeQueryBuilder) of
        Ok query ->
            let
                body =
                    query
                        |> Json.Encode.string
                        |> (\q -> Json.Encode.object [ ( "query", q ) ])
                        |> Http.jsonBody

                decoder =
                    getDecoder decodableQuery |> Json.Decode.at [ "data" ]

                request =
                    Http.post "/" body decoder
            in
                Http.toTask request |> Task.mapError HttpError

        Err errs ->
            Task.fail (InvalidQueryError errs)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( Nothing
    , performStarWarsQuery starWarsQuery |> Task.attempt ReceiveQueryResponse
    )


view : Model -> Html Msg
view model =
    div []
        [ model |> toString |> text ]


update : Msg -> Model -> ( Model, Cmd Msg )
update (ReceiveQueryResponse response) model =
    Just response ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
