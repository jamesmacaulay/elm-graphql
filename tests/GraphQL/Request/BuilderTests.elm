module GraphQL.Request.BuilderTests exposing (..)

import Test exposing (..)
import Expect
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Response as Response
import Json.Decode as Decode
import Json.Encode as Encode
import Dict exposing (Dict)


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
                |> (\decoder -> Decode.decodeString decoder testJSON)
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
    , createdAt : FakeTime
    , projects : Maybe (List ExampleQueryProject)
    }


type ExampleQueryProjectId
    = ExampleQueryProjectId String


type alias ExampleQueryProject =
    { id : ExampleQueryProjectId
    , name : String
    , featured : Bool
    , secrecyLevel : Maybe Int
    , selected : Bool
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


type FakeTimeType
    = FakeTimeType


type FakeTime
    = FakeTime Int


fakeTime : ValueSpec NonNull FakeTimeType FakeTime vars
fakeTime =
    Decode.string
        |> Decode.andThen
            (\timeString ->
                case timeString of
                    "2017-04-02T19:57:00Z" ->
                        Decode.succeed (FakeTime 1491163020000)

                    _ ->
                        Decode.fail "I only know how to parse one string"
            )
        |> customScalar FakeTimeType


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
                            |> withLocalConstant False
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
                    |> with (aliasAs "creationTime" (field "createdAt" [] fakeTime))
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


type alias UpdateUserInputData =
    { id : String
    , email : Maybe String
    , phoneNumber : Maybe (Maybe String)
    }


userDataVar : Var.Variable { vars | userData : UpdateUserInputData }
userDataVar =
    Var.required "userData"
        .userData
        (Var.object "UpdateUserInput"
            [ Var.field "id" .id Var.id
            , Var.optionalField "email" .email Var.string
            , Var.optionalField "phoneNumber" .phoneNumber (Var.nullable Var.string)
            ]
        )


optionalFieldVariableMutationDocument : Document Mutation String { userData : UpdateUserInputData }
optionalFieldVariableMutationDocument =
    mutationDocument <|
        extract
            (field "updateUser"
                [ ( "userData", Arg.variable userDataVar ) ]
                (extract (field "updateToken" [] string))
            )


userAvatarUrlField : String -> SelectionSpec Field String vars
userAvatarUrlField name =
    aliasAs ("user_" ++ name) <|
        field "user"
            [ ( "login", Arg.string name ) ]
            (extract (field "avatarUrl" [] string))


projectLogoUrlField : String -> SelectionSpec Field String vars
projectLogoUrlField projectId =
    aliasAs ("project_" ++ projectId) <|
        field "project"
            [ ( "id", Arg.string projectId ) ]
            (extract (field "logoUrl" [] string))


exampleKeyValuePairsSelections : List (SelectionSpec Field String vars)
exampleKeyValuePairsSelections =
    [ userAvatarUrlField "alice"
    , userAvatarUrlField "bob"
    , userAvatarUrlField "charlie"
    , projectLogoUrlField "123"
    , projectLogoUrlField "456"
    ]


exampleKeyValuePairsQueryRequest : Request Query (List ( String, String ))
exampleKeyValuePairsQueryRequest =
    exampleKeyValuePairsSelections
        |> keyValuePairs
        |> queryDocument
        |> request ()


exampleDictQueryRequest : Request Query (Dict String String)
exampleDictQueryRequest =
    exampleKeyValuePairsSelections
        |> dict
        |> queryDocument
        |> request ()


exampleKeyValuePairsQueryRequestExpectedBody : String
exampleKeyValuePairsQueryRequestExpectedBody =
    """query {
  user_alice: user(login: "alice") {
    avatarUrl
  }
  user_bob: user(login: "bob") {
    avatarUrl
  }
  user_charlie: user(login: "charlie") {
    avatarUrl
  }
  project_123: project(id: "123") {
    logoUrl
  }
  project_456: project(id: "456") {
    logoUrl
  }
}"""


exampleKeyValuePairsResponse : String
exampleKeyValuePairsResponse =
    """{
    "data": {
        "user_alice": {
            "avatarUrl": "https://cdn.example.com/user_alice.png"
        },
        "user_bob": {
            "avatarUrl": "https://cdn.example.com/user_bob.png"
        },
        "user_charlie": {
            "avatarUrl": "https://cdn.example.com/user_charlie.png"
        },
        "project_123": {
            "logoUrl": "https://cdn.example.com/project_123.png"
        },
        "project_456": {
            "logoUrl": "https://cdn.example.com/project_456.png"
        }
    }
}"""


exampleKeyValuePairsDecoded : List ( String, String )
exampleKeyValuePairsDecoded =
    [ ( "user_alice", "https://cdn.example.com/user_alice.png" )
    , ( "user_bob", "https://cdn.example.com/user_bob.png" )
    , ( "user_charlie", "https://cdn.example.com/user_charlie.png" )
    , ( "project_123", "https://cdn.example.com/project_123.png" )
    , ( "project_456", "https://cdn.example.com/project_456.png" )
    ]


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
                        (Decode.map3 (\a b c -> ( a, b, c ))
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
                            , createdAt = FakeTime 1491163020000
                            , projects =
                                Just
                                    [ { id = ExampleQueryProjectId "456"
                                      , name = "Top Secret Project"
                                      , featured = False
                                      , secrecyLevel = Just 9000
                                      , selected = False
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
    , test "mutation with omitted email and explicit null phoneNumber" <|
        \() ->
            optionalFieldVariableMutationDocument
                |> request
                    { userData =
                        { id = "user:123"
                        , email = Nothing
                        , phoneNumber = Just Nothing
                        }
                    }
                |> jsonVariableValues
                |> Maybe.map (Encode.encode 0)
                |> Expect.equal
                    (Just "{\"userData\":{\"id\":\"user:123\",\"phoneNumber\":null}}")
    , test "mutation with omitted email and included phoneNumber" <|
        \() ->
            optionalFieldVariableMutationDocument
                |> request
                    { userData =
                        { id = "user:123"
                        , email = Nothing
                        , phoneNumber = Just (Just "555-1212")
                        }
                    }
                |> jsonVariableValues
                |> Maybe.map (Encode.encode 0)
                |> Expect.equal
                    (Just "{\"userData\":{\"id\":\"user:123\",\"phoneNumber\":\"555-1212\"}}")
    , test "mutation with included email and omitted phoneNumber" <|
        \() ->
            optionalFieldVariableMutationDocument
                |> request
                    { userData =
                        { id = "user:123"
                        , email = Just "alice@example.com"
                        , phoneNumber = Nothing
                        }
                    }
                |> jsonVariableValues
                |> Maybe.map (Encode.encode 0)
                |> Expect.equal
                    (Just "{\"userData\":{\"id\":\"user:123\",\"email\":\"alice@example.com\"}}")
    , test "encoding a keyValuePairs query" <|
        \() ->
            exampleKeyValuePairsQueryRequest
                |> requestBody
                |> Expect.equal exampleKeyValuePairsQueryRequestExpectedBody
    , test "encoding a dict query" <|
        \() ->
            exampleDictQueryRequest
                |> requestBody
                |> Expect.equal exampleKeyValuePairsQueryRequestExpectedBody
    , test "decoding a keyValuePairs response" <|
        \() ->
            exampleKeyValuePairsResponse
                |> Decode.decodeString
                    (Decode.field "data" (responseDataDecoder exampleKeyValuePairsQueryRequest))
                |> Expect.equal (Ok exampleKeyValuePairsDecoded)
    , test "decoding a dict response" <|
        \() ->
            exampleKeyValuePairsResponse
                |> Decode.decodeString
                    (Decode.field "data" (responseDataDecoder exampleDictQueryRequest))
                |> Expect.equal (Ok (Dict.fromList exampleKeyValuePairsDecoded))
    , test "named query with arguments" <|
        \() ->
            (namedQueryDocument "MyQuery" (extract (field "foo" [ ( "bar", Var.required "bar" identity Var.bool |> Arg.variable ) ] string)))
                |> request True
                |> requestBody
                |> Expect.equal """query MyQuery ($bar: Boolean!) {
  foo(bar: $bar)
}"""
    , test "named mutation with no arguments" <|
        \() ->
            (namedMutationDocument "MyMutation" (extract (field "foo" [] string)))
                |> request True
                |> requestBody
                |> Expect.equal """mutation MyMutation {
  foo
}"""
    ]


all : Test.Test
all =
    describe "GraphQL.Request.Builder" tests
