module GraphQL.Request.BuilderTests exposing (..)

import Test exposing (..)
import Expect
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Value as Value
import GraphQL.Request.Builder.Variable as Variable
import GraphQL.Request.Builder.TypeRef as TypeRef
import GraphQL.Response as Response
import Json.Decode as Decode


testDecoder :
    String
    -> Request operationType variableSource result
    -> String
    -> result
    -> Test.Test
testDecoder expr request testJSON expectedResult =
    test ("Decoder for " ++ expr) <|
        \() ->
            request
                |> responseDecoder
                |> flip Decode.decodeString testJSON
                |> Expect.equal (Ok expectedResult)


type alias ExampleQueryRoot =
    { user : ExampleQueryUser
    }


type alias ExampleQueryUser =
    { id : String
    , name : String
    , role : ExampleRole
    , projects : List ExampleQueryProject
    }


type alias ExampleQueryProject =
    { id : String
    , name : String
    , featured : Bool
    }


type ExampleRole
    = ExampleAdminRole
    | ExampleMemberRole


type alias ExampleVariables =
    { userId : String
    , includeProjects : Maybe (Maybe Bool)
    }


exampleQueryRequest : Request Query ExampleVariables ExampleQueryRoot
exampleQueryRequest =
    object ExampleQueryRoot
        |> withField "user"
            [ args [ ( "id", Value.variable (Variable.required "userId" .userId Variable.string) ) ] ]
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
                |> withField "projects"
                    [ args [ ( "first", Value.int 1 ) ]
                    , directive "include" [ ( "if", Value.variable (Variable.optional "includeProjects" .includeProjects (Variable.nullable Variable.bool) (Just False)) ) ]
                    ]
                    (list
                        (object ExampleQueryProject
                            |> withField "id" [] id
                            |> withField "name" [] string
                            |> withField "featured" [] bool
                        )
                    )
            )
        |> queryDocument
        |> request
            { userId = "123"
            , includeProjects = Just (Just True)
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
                    "featured": false
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
                |> Expect.equal """query ($userId: String!, $includeProjects: Boolean = false) {
  user(id: $userId) {
    id
    name
    role
    projects(first: 1) @include(if: $includeProjects) {
      id
      name
      featured
    }
  }
}"""
    , test "variable values of a request" <|
        \() ->
            exampleQueryRequest
                |> requestVariableValues
                |> Expect.equal
                    [ ( "userId", Value.getAST (Value.string "123") )
                    , ( "includeProjects", Value.getAST (Value.bool True) )
                    ]
    , test "decoding a successful response of a request" <|
        \() ->
            exampleSuccessResponse
                |> Decode.decodeString (responseDecoder exampleQueryRequest)
                |> Expect.equal
                    (Ok
                        { user =
                            { id = "123"
                            , name = "alice"
                            , role = ExampleAdminRole
                            , projects =
                                [ { id = "456"
                                  , name = "Top Secret Project"
                                  , featured = False
                                  }
                                ]
                            }
                        }
                    )
    , test "decoding an error response of a request" <|
        \() ->
            exampleErrorResponse
                |> Decode.decodeString Response.errorsDecoder
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
