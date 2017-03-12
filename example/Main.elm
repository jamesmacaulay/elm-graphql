module Main exposing (..)

import Html exposing (Html, div, text)
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Value as Value
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Client.Http as GraphQLClient
import Task exposing (Task)


type alias FilmSummary =
    { title : Maybe String
    , someCharacterNames : List (Maybe String)
    , somePlanetNames : Maybe (List (Maybe String))
    }


connectionNodes : Spec nullability coreType variableSource result -> Spec NonNull ObjectType variableSource (List result)
connectionNodes spec =
    field "edges" [] (list (field "node" [] spec))


{-| The definition of `starWarsRequest` builds up a query request value that
will later be encoded into the following GraphQL query document:

fragment filmPlanetsFragment on Film {
  planetConnection(first: $pageSize) {
    edges {
      node {
        name
      }
    }
  }
}

query ($filmID: ID!, $pageSize: Int = 3) {
  film(filmID: $filmID) {
    title
    characterConnection(first: $pageSize) {
      edges {
        node {
          name
        }
      }
    }
    ...filmPlanetsFragment
  }
}

This query is sent along with variable values extracted from the record passed
to `request`, and the response is decoded into a `FilmSummary`.
-}
starWarsRequest : Request Query { filmID : String, pageSize : Maybe Int } FilmSummary
starWarsRequest =
    let
        filmID =
            Var.required "filmID" .filmID Var.id

        pageSize =
            Var.optional "pageSize" .pageSize (Var.nullable Var.int) (Just 3)

        planetsFragment =
            fragment "filmPlanetsFragment"
                (onType "Film")
                (field "planetConnection"
                    [ args [ ( "first", Value.variable pageSize ) ] ]
                    (connectionNodes (field "name" [] (nullable string)))
                )
    in
        field "film"
            [ args [ ( "filmID", Value.variable filmID ) ] ]
            (map3 FilmSummary
                (field "title" [] (nullable string))
                (field "characterConnection"
                    [ args [ ( "first", Value.variable pageSize ) ] ]
                    (connectionNodes (field "name" [] (nullable string)))
                )
                (fragmentSpread planetsFragment [])
            )
            |> queryDocument
            |> request
                { filmID = "1"
                , pageSize = Nothing
                }


type alias StarWarsResponse =
    Result GraphQLClient.Error FilmSummary


type alias Model =
    Maybe StarWarsResponse


type Msg
    = ReceiveQueryResponse StarWarsResponse


sendQueryRequest : Request Query variableSource a -> Task GraphQLClient.Error a
sendQueryRequest request =
    GraphQLClient.sendQuery "/" request


sendStarWarsQuery : Cmd Msg
sendStarWarsQuery =
    sendQueryRequest starWarsRequest
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
