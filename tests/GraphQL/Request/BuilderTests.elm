module GraphQL.Request.BuilderTests exposing (..)

import Test exposing (..)
import Expect
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Variable
import GraphQL.Request.Document.AST as AST
import GraphQL.Response as Response
import Json.Decode as Decode


testDecoder :
    String
    -> Request operationType result
    -> String
    -> result
    -> Test.Test
testDecoder expr request testJSON expectedResult =
    test ("Decoder for " ++ expr) <|
        \() ->
            request
                |> responseDataDecoder
                |> flip Decode.decodeString testJSON
                |> Expect.equal (Ok expectedResult)


type alias ExampleQueryRoot =
    { user : ExampleQueryUser
    }


type alias ExampleQueryUser =
    { id : String
    , name : String
    , role : ExampleRole
    , projects : Maybe (List ExampleQueryProject)
    }


type alias ExampleQueryProject =
    { id : String
    , name : String
    , featured : Bool
    , secrecyLevel : Maybe Int
    }


type ExampleRole
    = ExampleAdminRole
    | ExampleMemberRole


type alias ExampleVariables =
    { userId : String
    , includeProjects : Maybe Bool
    , secrecyUnits : Maybe String
    }


userIdVar : Variable.Variable { v | userId : String }
userIdVar =
    Variable.required
        "userId"
        .userId
        Variable.string


includeProjectsVar : Variable.Variable { v | includeProjects : Maybe Bool }
includeProjectsVar =
    Variable.optional
        "includeProjects"
        .includeProjects
        Variable.bool
        False


secrecyUnitsVar : Variable.Variable { v | secrecyUnits : Maybe String }
secrecyUnitsVar =
    Variable.optional
        "secrecyUnits"
        .secrecyUnits
        Variable.string
        "metric"


exampleQueryUserProjectsFragment : Fragment ExampleVariables (List ExampleQueryProject)
exampleQueryUserProjectsFragment =
    fragment "userProjectsFragment"
        (onType "User")
        (field "projects"
            [ args [ ( "first", Arg.int 1 ) ]
            , directive "include" [ ( "if", Arg.variable includeProjectsVar ) ]
            ]
            (list
                (object ExampleQueryProject
                    |> withField "id" [] id
                    |> withField "name" [] string
                    |> withField "featured" [] bool
                    |> withInlineFragment (Just (onType "SecretProject"))
                        []
                        (field "secrecyLevel"
                            [ args [ ( "units", Arg.variable secrecyUnitsVar ) ] ]
                            int
                        )
                )
            )
        )


exampleQueryRequest : Request Query ExampleQueryRoot
exampleQueryRequest =
    object ExampleQueryRoot
        |> withField "user"
            [ args [ ( "id", Arg.variable userIdVar ) ] ]
            (object ExampleQueryUser
                |> withField "id" [] id
                |> withField "name" [] string
                |> withField "role"
                    []
                    (enum
                        [ ( "ADMIN", ExampleAdminRole )
                        , ( "MEMBER", ExampleMemberRole )
                        ]
                    )
                |> withFragment exampleQueryUserProjectsFragment []
            )
        |> queryDocument
        |> request
            { userId = "123"
            , includeProjects = Just True
            , secrecyUnits = Nothing
            }


exampleSuccessResponse : String
exampleSuccessResponse =
    """{
    "data": {
        "user": {
            "id": "123",
            "name": "alice",
            "role": "ADMIN",
            "projects": [
                {
                    "id": "456",
                    "name": "Top Secret Project",
                    "featured": false,
                    "secrecyLevel": 9000
                }
            ]
        }
    }
}"""


exampleErrorResponse : String
exampleErrorResponse =
    """{
    "errors": [
        {
            "message": "Cannot query field \\"user\\" on type \\"Query\\".",
            "locations": [
                {
                    "line": 2,
                    "column": 3
                }
            ]
        }
    ]
}"""


tests : List Test.Test
tests =
    [ test "encoding a request" <|
        \() ->
            exampleQueryRequest
                |> requestBody
                |> Expect.equal """fragment userProjectsFragment on User {
  projects(first: 1) @include(if: $includeProjects) {
    id
    name
    featured
    ... on SecretProject {
      secrecyLevel(units: $secrecyUnits)
    }
  }
}

query ($userId: String!, $includeProjects: Boolean = false, $secrecyUnits: String = "metric") {
  user(id: $userId) {
    id
    name
    role
    ...userProjectsFragment
  }
}"""
    , test "variable values of a request" <|
        \() ->
            exampleQueryRequest
                |> jsonVariableValues
                |> Maybe.map
                    (Decode.decodeValue
                        (Decode.map2 (,)
                            (Decode.field "userId" Decode.string)
                            (Decode.field "includeProjects" Decode.bool)
                        )
                    )
                |> Expect.equal
                    (Just (Ok ( "123", True )))
    , test "decoding a successful response of a request" <|
        \() ->
            exampleSuccessResponse
                |> Decode.decodeString
                    (Decode.field "data" (responseDataDecoder exampleQueryRequest))
                |> Expect.equal
                    (Ok
                        { user =
                            { id = "123"
                            , name = "alice"
                            , role = ExampleAdminRole
                            , projects =
                                Just
                                    [ { id = "456"
                                      , name = "Top Secret Project"
                                      , featured = False
                                      , secrecyLevel = Just 9000
                                      }
                                    ]
                            }
                        }
                    )
    , test "decoding an error response of a request" <|
        \() ->
            exampleErrorResponse
                |> Decode.decodeString (Decode.field "errors" Response.errorsDecoder)
                |> Expect.equal
                    (Ok
                        [ { message = "Cannot query field \"user\" on type \"Query\"."
                          , locations =
                                [ { line = 2
                                  , column = 3
                                  }
                                ]
                          }
                        ]
                    )
    ]


all : Test.Test
all =
    describe "GraphQL.Request.Builder" tests
