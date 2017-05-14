module GraphQL.Request.BuilderTests exposing (..)

import Test exposing (..)
import Expect
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Response as Response
import Json.Decode as Decode
import Date exposing (Date)
import Time exposing (Time)


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


type ExampleQueryUserId
    = ExampleQueryUserId String


type alias ExampleQueryUser =
    { id : ExampleQueryUserId
    , name : String
    , role : ExampleRole
    , createdAt : Time
    , projects : Maybe (List ExampleQueryProject)
    }


type ExampleQueryProjectId
    = ExampleQueryProjectId String


type alias ExampleQueryProject =
    { id : ExampleQueryProjectId
    , name : String
    , featured : Bool
    , secrecyLevel : Maybe Int
    }


type ExampleRole
    = ExampleAdminRole
    | ExampleMemberRole


type ExampleNameKind
    = ExampleFirstName
    | ExampleFullName


type alias ExampleVariables =
    { userId : String
    , userNameKind : ExampleNameKind
    , includeProjects : Maybe Bool
    , projectIds : List String
    , secrecyUnits : Maybe String
    }


type TimeType
    = TimeType


time : ValueSpec NonNull TimeType Time vars
time =
    Decode.string
        |> Decode.andThen
            (\timeString ->
                case Result.map Date.toTime (Date.fromString timeString) of
                    Ok time ->
                        Decode.succeed time

                    Err errorMessage ->
                        Decode.fail errorMessage
            )
        |> customScalar TimeType


userIdVar : Var.Variable { v | userId : String }
userIdVar =
    Var.required
        "userId"
        .userId
        Var.string


exampleNameKindToEnumSymbol : ExampleNameKind -> String
exampleNameKindToEnumSymbol kind =
    case kind of
        ExampleFirstName ->
            "FIRST_NAME"

        ExampleFullName ->
            "FULL_NAME"


userNameKindVar : Var.Variable { v | userNameKind : ExampleNameKind }
userNameKindVar =
    Var.required
        "userNameKind"
        .userNameKind
        (Var.enum "NameKind" exampleNameKindToEnumSymbol)


includeProjectsVar : Var.Variable { v | includeProjects : Maybe Bool }
includeProjectsVar =
    Var.optional
        "includeProjects"
        .includeProjects
        Var.bool
        False


projectIdsVar : Var.Variable { v | projectIds : List String }
projectIdsVar =
    Var.required
        "projectIds"
        .projectIds
        (Var.list Var.id)


secrecyUnitsVar : Var.Variable { v | secrecyUnits : Maybe String }
secrecyUnitsVar =
    Var.optional
        "secrecyUnits"
        .secrecyUnits
        Var.string
        "metric"


exampleQueryUserProjectsFragment : Fragment (Maybe (List ExampleQueryProject)) ExampleVariables
exampleQueryUserProjectsFragment =
    fragment "userProjectsFragment"
        (onType "User")
        (extract
            (withDirectives [ ( "include", [ ( "if", Arg.variable includeProjectsVar ) ] ) ]
                (field "projects"
                    [ ( "first", Arg.int 1 ), ( "ids", Arg.variable projectIdsVar ) ]
                    (list
                        (object ExampleQueryProject
                            |> with (field "id" [] (map ExampleQueryProjectId id))
                            |> with (field "name" [] string)
                            |> with (field "featured" [] bool)
                            |> with
                                (inlineFragment (Just (onType "SecretProject"))
                                    (extract
                                        (field "secrecyLevel"
                                            [ ( "units", Arg.variable secrecyUnitsVar ) ]
                                            int
                                        )
                                    )
                                )
                        )
                    )
                )
            )
        )


roleEnum : ValueSpec NonNull EnumType ExampleRole vars
roleEnum =
    enum
        [ ( "ADMIN", ExampleAdminRole )
        , ( "MEMBER", ExampleMemberRole )
        ]


exampleQueryRequest : Request Query ExampleQueryRoot
exampleQueryRequest =
    object ExampleQueryRoot
        |> with
            (field "user"
                [ ( "id", Arg.variable userIdVar )
                , ( "duplicateUserIdTest", Arg.variable userIdVar )
                , ( "intListTest"
                  , [ 1, 2, 3 ]
                        |> List.map Arg.int
                        |> Arg.list
                  )
                , ( "stringListTest"
                  , [ "foo", "bar", "baz" ]
                        |> List.map Arg.string
                        |> Arg.list
                  )
                ]
                (object ExampleQueryUser
                    |> with (field "id" [] (map ExampleQueryUserId id))
                    |> with
                        (field "name"
                            [ ( "kind", Arg.variable userNameKindVar )
                            , ( "duplicateUserIdTest", Arg.variable userIdVar )
                            ]
                            string
                        )
                    |> with (field "role" [] roleEnum)
                    |> with (aliasAs "creationTime" (field "createdAt" [] time))
                    |> with (assume (fragmentSpread exampleQueryUserProjectsFragment))
                )
            )
        |> queryDocument
        |> request
            { userId = "123"
            , userNameKind = ExampleFirstName
            , includeProjects = Just True
            , projectIds = [ "456" ]
            , secrecyUnits = Nothing
            }


exampleQueryRequestExpectedBody : String
exampleQueryRequestExpectedBody =
    """fragment userProjectsFragment on User {
  projects(first: 1, ids: $projectIds) @include(if: $includeProjects) {
    id
    name
    featured
    ... on SecretProject {
      secrecyLevel(units: $secrecyUnits)
    }
  }
}

query ($userId: String!, $userNameKind: NameKind!, $includeProjects: Boolean = false, $projectIds: [ID!]!, $secrecyUnits: String = "metric") {
  user(id: $userId, duplicateUserIdTest: $userId, intListTest: [1, 2, 3], stringListTest: ["foo", "bar", "baz"]) {
    id
    name(kind: $userNameKind, duplicateUserIdTest: $userId)
    role
    creationTime: createdAt
    ...userProjectsFragment
  }
}"""


exampleSuccessResponse : String
exampleSuccessResponse =
    """{
    "data": {
        "user": {
            "id": "123",
            "name": "alice",
            "role": "ADMIN",
            "creationTime": "2017-04-02T19:57:00Z",
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


type alias LoginVars =
    { username : String
    , password : String
    }


exampleMutationDocument : Document Mutation String LoginVars
exampleMutationDocument =
    let
        usernameVar =
            Var.required "username" .username Var.string

        passwordVar =
            Var.required "password" .password Var.string
    in
        mutationDocument <|
            extract
                (field "login"
                    [ ( "username", Arg.variable usernameVar )
                    , ( "password", Arg.variable passwordVar )
                    ]
                    (extract (field "token" [] string))
                )


exampleMutationRequest : Request Mutation String
exampleMutationRequest =
    exampleMutationDocument
        |> request
            { username = "alice"
            , password = "IaFkVmT3EK"
            }


tests : List Test.Test
tests =
    [ test "encoding a request" <|
        \() ->
            exampleQueryRequest
                |> requestBody
                |> Expect.equal exampleQueryRequestExpectedBody
    , test "variable values of a request" <|
        \() ->
            exampleQueryRequest
                |> jsonVariableValues
                |> Maybe.map
                    (Decode.decodeValue
                        (Decode.map3 (,,)
                            (Decode.field "userId" Decode.string)
                            (Decode.field "userNameKind" Decode.string)
                            (Decode.field "includeProjects" Decode.bool)
                        )
                    )
                |> Expect.equal
                    (Just (Ok ( "123", "FIRST_NAME", True )))
    , test "decoding a successful response of a request" <|
        \() ->
            exampleSuccessResponse
                |> Decode.decodeString
                    (Decode.field "data" (responseDataDecoder exampleQueryRequest))
                |> Expect.equal
                    (Ok
                        { user =
                            { id = ExampleQueryUserId "123"
                            , name = "alice"
                            , role = ExampleAdminRole
                            , createdAt = 1491163020000
                            , projects =
                                Just
                                    [ { id = ExampleQueryProjectId "456"
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
    , test "mutation request serialization" <|
        \() ->
            exampleMutationRequest
                |> requestBody
                |> Expect.equal """mutation ($username: String!, $password: String!) {
  login(username: $username, password: $password) {
    token
  }
}"""
    ]


all : Test.Test
all =
    describe "GraphQL.Request.Builder" tests
