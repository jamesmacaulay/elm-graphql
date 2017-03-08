module Main exposing (..)

import Html exposing (Html, div, text)
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Value as Value
import GraphQL.Client.Http as GraphQLClient
import Task exposing (Task)


type alias FilmSummary =
    { title : Maybe String
    , someCharacterNames : List (Maybe String)
    }


connectionNodes : Spec nullability coreType result -> Spec NonNull ObjectType (List result)
connectionNodes spec =
    field "edges" [] (list (field "node" [] spec))


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
starWarsRequest : Request Query FilmSummary
starWarsRequest =
    field "film"
        [ args [ ( "filmID", Value.int 1 ) ] ]
        (map2 FilmSummary
            (field "title" [] (nullable string))
            (field "characterConnection"
                [ args [ ( "first", Value.int 3 ) ] ]
                (connectionNodes (field "name" [] (nullable string)))
            )
        )
        |> queryDocument []
        |> request []


type alias StarWarsResponse =
    Result GraphQLClient.Error FilmSummary


type alias Model =
    Maybe StarWarsResponse


type Msg
    = ReceiveQueryResponse StarWarsResponse


queryRequest : Request Query a -> Task GraphQLClient.Error a
queryRequest request =
    GraphQLClient.sendQuery "/" request


sendStarWarsQuery : Cmd Msg
sendStarWarsQuery =
    queryRequest starWarsRequest
        |> Task.attempt ReceiveQueryResponse


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
    ( Nothing, sendStarWarsQuery )


view : Model -> Html Msg
view model =
    div []
        [ model |> toString |> text ]


update : Msg -> Model -> ( Model, Cmd Msg )
update (ReceiveQueryResponse response) model =
    ( Just response, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
