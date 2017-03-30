module Main exposing (..)

import Html exposing (Html, div, text)
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Client.Http as GraphQLClient
import Task exposing (Task)


{-| Responses to `starWarsRequest` are decoded into this type.
-}
type alias FilmSummary =
    { title : Maybe String
    , someCharacterNames : List (Maybe String)
    , somePlanetNames : Maybe (List (Maybe String))
    }


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
starWarsRequest : Request Query FilmSummary
starWarsRequest =
    let
        filmID =
            Var.required "filmID" .filmID Var.id

        pageSize =
            Var.optional "pageSize" .pageSize (Var.nullable Var.int) (Just 3)

        planetsFragment =
            fragment "filmPlanetsFragment"
                (onType "Film")
                (extract
                    (field "planetConnection"
                        [ ( "first", Arg.variable pageSize ) ]
                        (connectionNodes (extract (field "name" [] (nullable string))))
                    )
                )
    in
        extract
            (field "film"
                [ ( "filmID", Arg.variable filmID ) ]
                (object FilmSummary
                    |> with (field "title" [] (nullable string))
                    |> with
                        (field "characterConnection"
                            [ ( "first", Arg.variable pageSize ) ]
                            (connectionNodes (extract (field "name" [] (nullable string))))
                        )
                    |> with (fragmentSpread planetsFragment)
                )
            )
            |> queryDocument
            |> request
                { filmID = "1"
                , pageSize = Nothing
                }


{-| A function that helps you extract node objects from paginated Relay connections.
-}
connectionNodes :
    ValueSpec NonNull ObjectType result vars
    -> ValueSpec NonNull ObjectType (List result) vars
connectionNodes spec =
    extract
        (field "edges"
            []
            (list
                (extract
                    (field "node" [] spec)
                )
            )
        )


type alias StarWarsResponse =
    Result GraphQLClient.Error FilmSummary


type alias Model =
    Maybe StarWarsResponse


type Msg
    = ReceiveQueryResponse StarWarsResponse


sendQueryRequest : Request Query a -> Task GraphQLClient.Error a
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
