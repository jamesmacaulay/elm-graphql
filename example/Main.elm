module Main exposing (..)

import Html exposing (Html, div, text)
import Html.App
import GraphQL.Query exposing (..)
import GraphQL.Query.Encode exposing (encodeQuery)
import Json.Decode
import Json.Encode
import Http
import Task exposing (Task)


type alias FilmSummary =
    { title : Maybe String
    , someCharacterNames : List (Maybe String)
    }


extractConnectionNodes : Decodable ValueSpec a -> Decodable ValueSpec (List a)
extractConnectionNodes valueSpec =
    (extractField "edges" [] (list (extractField "node" [] valueSpec)))


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
starWarsQuery : Decodable Query FilmSummary
starWarsQuery =
    extractField "film"
        [ fieldArgs [ ( "filmID", int' 1 ) ] ]
        (object FilmSummary
            |> withField "title" [] (maybe string)
            |> withField "characterConnection"
                [ fieldArgs [ ( "first", int' 3 ) ] ]
                (extractConnectionNodes (extractField "name" [] (maybe string)))
        )
        |> query []


type alias Model =
    Maybe (Result Http.Error FilmSummary)


type Msg
    = ReceiveQueryResponse (Result Http.Error FilmSummary)


performStarWarsQuery : Decodable Query a -> Task Http.Error a
performStarWarsQuery decodableQuery =
    let
        body =
            decodableQuery
                |> getNode
                |> encodeQuery
                |> Json.Encode.string
                |> (\q -> Json.Encode.object [ ( "query", q ) ])
                |> Json.Encode.encode 0
                |> Http.string

        request =
            { verb = "POST"
            , headers =
                [ ( "Content-Type", "application/json" )
                ]
            , url = "/"
            , body = body
            }

        decoder =
            getDecoder decodableQuery |> Json.Decode.at [ "data" ]
    in
        Http.send Http.defaultSettings request
            |> Http.fromJson decoder


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    Nothing
        ! [ performStarWarsQuery starWarsQuery
                |> Task.perform (Err >> ReceiveQueryResponse) (Ok >> ReceiveQueryResponse)
          ]


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
