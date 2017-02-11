module Main exposing (..)

import Html exposing (Html, div, text)
import GraphQL.Query.Builder exposing (..)
import GraphQL.Query.Builder.Arg as Arg
import GraphQL.Client.Http as GraphQLClient
import Http


type alias FilmSummary =
    { title : Maybe String
    , someCharacterNames : List (Maybe String)
    }


connectionNodes : Spec a -> Spec (List a)
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
starWarsQuery : Query FilmSummary
starWarsQuery =
    field "film"
        [ fieldArgs [ ( "filmID", Arg.int 1 ) ] ]
        (produce FilmSummary
            |> withField "title" [] (nullable string)
            |> withField "characterConnection"
                [ fieldArgs [ ( "first", Arg.int 3 ) ] ]
                (connectionNodes (field "name" [] (nullable string)))
        )
        |> query []


type alias Model =
    Maybe (Result Http.Error FilmSummary)


type Msg
    = ReceiveQueryResponse (Result Http.Error FilmSummary)


queryRequest : Query a -> Http.Request a
queryRequest query =
    GraphQLClient.queryRequest "/" query Nothing


sendStarWarsQuery : Cmd Msg
sendStarWarsQuery =
    queryRequest starWarsQuery
        |> Http.send ReceiveQueryResponse


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
    Just response ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
