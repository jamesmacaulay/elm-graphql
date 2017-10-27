module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import GraphQL.Request.Document.Parser as Parser
import GraphQL.Request.Document.AST as AST
import Combine exposing (ParseErr, ParseOk)


type alias Model =
    { queryInput : String
    , parseResult : Result (ParseErr ()) (ParseOk () AST.Document)
    }


type Msg
    = ParseQueryInput
    | UpdateQueryInput String


main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }


exampleQuery : String
exampleQuery =
    """
{
  user(id: "abc123") {
    id
    name
    email
    projects(first: 10) {
      id
      name
    }
  }
}
"""


init : Model
init =
    { queryInput = exampleQuery
    , parseResult = Parser.parseDocument exampleQuery
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ParseQueryInput ->
            { model | parseResult = Parser.parseDocument model.queryInput }

        UpdateQueryInput text ->
            { model | queryInput = text }


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.div
            []
            [ Html.form [ Events.onSubmit ParseQueryInput ]
                [ Html.textarea
                    [ Events.onInput UpdateQueryInput
                    , Attr.style [ ( "width", "300px" ), ( "height", "300px" ) ]
                    ]
                    [ Html.text model.queryInput ]
                , Html.input
                    [ Attr.type_ "submit" ]
                    [ Html.text "Parse" ]
                ]
            ]
        , Html.div
            []
            [ Html.text (toString model.parseResult) ]
        ]
