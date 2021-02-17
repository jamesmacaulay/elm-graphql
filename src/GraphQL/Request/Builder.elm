module GraphQL.Request.Builder exposing
    ( ValueSpec, NonNull, Nullable, IntType, FloatType, StringType, BooleanType, IdType, EnumType, ListType, ObjectType
    , object, SelectionSpec, with, withLocalConstant, extract, assume, withDirectives, keyValuePairs, dict
    , Field, field, aliasAs
    , Fragment, FragmentSpread, InlineFragment, TypeCondition, fragment, onType, fragmentSpread, inlineFragment
    , int, float, string, bool, id, enum, enumWithDefault, customScalar
    , nullable
    , list
    , map
    , Document, Query, queryDocument, namedQueryDocument, Mutation, mutationDocument, namedMutationDocument
    , Request, request, requestBody, jsonVariableValues, responseDataDecoder
    )

{-| This module provides an interface for building up GraphQL requests in a way that gives you everything you need to safely and conveniently integrate them with your Elm program:

  - GraphQL variables are automatically converted from corresponding Elm types, so the compiler will let you know if there's a mismatch between the variables used in a document and the values you provide when you send the request.
  - Responses from the server are decoded using a `Json.Decode.Decoder` value that is built up as you build each part of the request document.

In order to use arguments and variables in your requests, you will need to use functions from the [`GraphQL.Request.Builder.Value`](GraphQL-Request-Builder-Value) and [`GraphQL.Request.Builder.Variable`](GraphQL-Request-Builder-Variable) modules. To send your requests over HTTP, see the [`GraphQL.Client.Http`](GraphQL-Client-Http) module.


# Specifying the structure of values

@docs ValueSpec, NonNull, Nullable, IntType, FloatType, StringType, BooleanType, IdType, EnumType, ListType, ObjectType


## Objects and selections

@docs object, SelectionSpec, with, withLocalConstant, extract, assume, withDirectives, keyValuePairs, dict


### Fields

@docs Field, field, aliasAs


### Fragments

@docs Fragment, FragmentSpread, InlineFragment, TypeCondition, fragment, onType, fragmentSpread, inlineFragment


## Scalars

@docs int, float, string, bool, id, enum, enumWithDefault, customScalar


## Nullability

@docs nullable


## Lists

@docs list


# Customizing the decoding process

@docs map


# Documents

@docs Document, Query, queryDocument, namedQueryDocument, Mutation, mutationDocument, namedMutationDocument


# Requests

@docs Request, request, requestBody, jsonVariableValues, responseDataDecoder

-}

import Dict exposing (Dict)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Variable exposing (Variable)
import GraphQL.Request.Builder.Variable.Util as VarUtil
import GraphQL.Request.Document.AST as AST
import GraphQL.Request.Document.AST.Serialize as Serialize
import GraphQL.Request.Document.AST.Util as Util
import GraphQL.Request.Document.AST.Value.Json.Encode as ValueEncode
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)


{-| Specifies a named object, interface, or union type from the GraphQL schema that a fragment or inline fragment is valid to be used with.
-}
type alias TypeCondition =
    AST.TypeCondition


{-| A `Request` bundles a `Document` along with any variable values that are to be sent along with it to the server. The `operationType` parameter may be either `Query` or `Mutation`. The `result` parameter is the type that a successful response from the server is decoded into.
-}
type Request operationType result
    = Request
        { documentAST : AST.Document
        , documentString : String
        , variableValues : List ( String, AST.ConstantValue )
        , responseDataDecoder : Decoder result
        }


{-| A `Document` represents a single-operation GraphQL request document, along with the information necessary to encode variable values used in the document and to decode successful responses from the server. The `vars` parameter is the type of value that must be provided when constructing a `Request` in order for the document's variable values to be obtained. The `operationType` and `result` parameters are the same as in the `Request` type.
-}
type Document operationType result vars
    = Document
        { operation : Operation operationType result vars
        , ast : AST.Document
        , serialized : String
        }


type Operation operationType result vars
    = Operation
        { operationType : OperationType operationType
        , name : Maybe String
        , directives : List ( String, List ( String, Arg.Value vars ) )
        , spec : ValueSpec NonNull ObjectType result vars
        }


type OperationType operationType
    = QueryOperationType
    | MutationOperationType


{-| This type is used as a marker for `Request` and `Document` types to indicate that the document's operation is a query.
-}
type Query
    = Query


{-| This type is used as a marker for `Request` and `Document` types to indicate that the document's operation is a mutation.
-}
type Mutation
    = Mutation


{-| A fragment definition. The `vars` parameter specifies the type of the Elm value required to supply any variables used anywhere within the fragment. The `result` parameter is the type of the Elm value obtained from the `Fragment`'s JSON decoder.
-}
type Fragment result vars
    = Fragment
        { name : String
        , typeCondition : TypeCondition
        , directives : List ( String, List ( String, Arg.Value vars ) )
        , spec : ValueSpec NonNull ObjectType result vars
        }


{-| A `ValueSpec` is a structured way of describing a value that you want back from a GraphQL server, and it is the fundamental building block of the request builder interface provided by this module. It corresponds loosely with the GraphQL concept of the "selection set", but it is used for scalar values as well as object values, and holds more information about their expected types.

The `nullability` and `coreType` parameters are used by various functions in this module to ensure consistency when combining `ValueSpec` values. As such, they will probably only become relevant to you when reading error messages from the compiler, at which point they will hopefully make the situation easier to understand.

The `result` parameter specifies the type produced by the JSON decoder of the `ValueSpec`.

The `vars` parameter specifies the type of the Elm value required to supply any variables used anywhere within the `ValueSpec`.

-}
type ValueSpec nullability coreType result vars
    = ValueSpec (SourceType nullability coreType) (AST.SelectionSet -> Decoder result) (List (Variable vars)) (List AST.FragmentDefinitionInfo)


{-| A specification for a GraphQL selection, to be added to an object `ValueSpec` using the `with` function.

The `selectionType` can be either `Field`, `FragmentSpread`, or `InlineFragment`.

The `result` parameter specifies the type produced by the JSON decoder of the `SelectionSpec`.

The `vars` parameter specifies the type of the Elm value required to supply any variables used anywhere within the `SelectionSpec`.

-}
type SelectionSpec selectionType result vars
    = SelectionSpec AST.Selection (AST.SelectionSet -> Decoder result) (List (Variable vars)) (List AST.FragmentDefinitionInfo)


selectionDecoder :
    AST.Selection
    -> (AST.SelectionSet -> Decoder result)
    -> AST.SelectionSet
    -> Decoder result
selectionDecoder selectionAST decoder =
    case selectionAST of
        AST.Field fieldInfo ->
            Decode.field (Util.responseKey fieldInfo) << decoder

        _ ->
            decoder


{-| Indicates that a `SelectionSpec` represents a GraphQL field.
-}
type Field
    = Field


{-| Indicates that a `SelectionSpec` represents a GraphQL fragment spread.
-}
type FragmentSpread
    = FragmentSpread


{-| Indicates that a `SelectionSpec` represents a GraphQL inline fragment.
-}
type InlineFragment
    = InlineFragment


type SourceType nullability coreType
    = SpecifiedType (SpecifiedTypeInfo nullability coreType)
    | AnyType


type alias SpecifiedTypeInfo nullability coreType =
    { nullability : Nullability nullability
    , coreType : coreType
    , join : coreType -> coreType -> coreType
    , selectionSet : AST.SelectionSet
    }


{-| Indicates that a `ValueSpec` describes GraphQL values that may be `null`.
-}
type Nullable
    = Nullable


{-| Indicates that a `ValueSpec` describes GraphQL values that may not be `null`. Unlike in the GraphQL schema language, `NonNull` is the default in this library.
-}
type NonNull
    = NonNull


type Nullability a
    = NullableFlag
    | NonNullFlag


nullableFlag : Nullability Nullable
nullableFlag =
    NullableFlag


nonNullFlag : Nullability NonNull
nonNullFlag =
    NonNullFlag


{-| Indicates that a `ValueSpec` describes GraphQL `Int` values.
-}
type IntType
    = IntType


{-| Indicates that a `ValueSpec` describes GraphQL `Float` values.
-}
type FloatType
    = FloatType


{-| Indicates that a `ValueSpec` describes GraphQL `String` values.
-}
type StringType
    = StringType


{-| Indicates that a `ValueSpec` describes GraphQL `Boolean` values.
-}
type BooleanType
    = BooleanType


{-| Indicates that a `ValueSpec` describes GraphQL `ID` values.
-}
type IdType
    = IdType


{-| Indicates that a `ValueSpec` describes values of some GraphQL Enum type.
-}
type EnumType
    = EnumType (List String)


{-| Indicates that a `ValueSpec` describes values of some GraphQL List type.
-}
type ListType itemNullability itemCoreType
    = ListType (SourceType itemNullability itemCoreType)


{-| Indicates that a `ValueSpec` describes values of some GraphQL Object type.
-}
type ObjectType
    = ObjectType


{-| Turn a `Document` into a `Request` that can be sent to a server, by supplying a `vars` value that is used to obtain values for any variables used in the `Document`. If the `Document` does not use any variables, then you can pass in `()` or any other value as the `vars` and it will be ignored.
-}
request :
    vars
    -> Document operationType result vars
    -> Request operationType result
request vars ((Document { operation, ast, serialized }) as doc) =
    Request
        { documentAST = ast
        , documentString = serialized
        , variableValues =
            documentVariables doc
                |> Variable.extractValuesFrom vars
        , responseDataDecoder = documentResponseDecoder doc
        }


{-| Get the serialized document body of a `Request`.
-}
requestBody : Request operationType result -> String
requestBody (Request requestRecord) =
    requestRecord.documentString


variableValuesToJson : List ( String, AST.ConstantValue ) -> Maybe Encode.Value
variableValuesToJson kvPairs =
    if List.isEmpty kvPairs then
        Nothing

    else
        kvPairs
            |> List.map (Tuple.mapSecond ValueEncode.encode)
            |> Encode.object
            |> Just


{-| Get the variable values associated with a `Request` (if there are any) as a `Maybe Json.Encode.Value`, ready to be sent as a parameter to a GraphQL server.
-}
jsonVariableValues : Request operationType result -> Maybe Encode.Value
jsonVariableValues (Request { variableValues }) =
    variableValuesToJson variableValues


{-| Get a JSON decoder that can be used to decode the data contained in a successful response to a `Request`. If you're working with a conventional GraphQL response over HTTP, the returned `Decoder` works on the data found under the `"data"` key of the response.
-}
responseDataDecoder : Request operationType result -> Decoder result
responseDataDecoder (Request requestRecord) =
    requestRecord.responseDataDecoder


fragmentDefinitionsFromOperation : Operation operationType result vars -> List AST.FragmentDefinitionInfo
fragmentDefinitionsFromOperation (Operation { spec }) =
    let
        (ValueSpec _ _ _ fragments) =
            spec
    in
    fragments


document : Operation operationType result vars -> Document operationType result vars
document operation =
    let
        fragmentDefinitions =
            fragmentDefinitionsFromOperation operation

        ast =
            AST.Document
                (List.map AST.FragmentDefinition fragmentDefinitions
                    ++ [ AST.OperationDefinition (operationAST operation) ]
                )
    in
    Document
        { operation = operation
        , ast = ast
        , serialized = Serialize.serializeDocument ast
        }


{-| Take a `ValueSpec` and return a `Document` for a single query operation. The argument must be a `NonNull Object` ValueSpec, because it represents the root-level selection set of the query operation.
-}
queryDocument :
    ValueSpec NonNull ObjectType result vars
    -> Document Query result vars
queryDocument spec =
    document
        (Operation
            { operationType = queryOperationType
            , name = Nothing
            , directives = []
            , spec = spec
            }
        )


{-| Like `queryDocument`, but takes a name for the query as an extra first argument. The name is used as a label for the operation in the generated GraphQL document and can be useful for logging and debugging purposes.
-}
namedQueryDocument :
    String
    -> ValueSpec NonNull ObjectType result vars
    -> Document Query result vars
namedQueryDocument queryName spec =
    document
        (Operation
            { operationType = queryOperationType
            , name = Just queryName
            , directives = []
            , spec = spec
            }
        )


queryOperationType : OperationType Query
queryOperationType =
    QueryOperationType


{-| Take a `ValueSpec` and return a `Document` for a single mutation operation. The argument must be a `NonNull Object` ValueSpec, because it represents the root-level selection set of the mutation operation. Here's an example of a mutation that logs in a user and extracts an auth token from the response:

    type alias LoginVars =
        { username : String
        , password : String
        }

    loginMutation : Document Mutation String LoginVars
    loginMutation =
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

-}
mutationDocument :
    ValueSpec NonNull ObjectType result vars
    -> Document Mutation result vars
mutationDocument spec =
    document
        (Operation
            { operationType = mutationOperationType
            , name = Nothing
            , directives = []
            , spec = spec
            }
        )


{-| Like `mutationDocument`, but takes a name for the mutation as an extra first argument. The name is used as a label for the operation in the generated GraphQL document and can be useful for logging and debugging purposes.
-}
namedMutationDocument :
    String
    -> ValueSpec NonNull ObjectType result vars
    -> Document Mutation result vars
namedMutationDocument mutationName spec =
    document
        (Operation
            { operationType = mutationOperationType
            , name = Just mutationName
            , directives = []
            , spec = spec
            }
        )


mutationOperationType : OperationType Mutation
mutationOperationType =
    MutationOperationType


{-| Construct a `Fragment` by providing a name, a `TypeCondition`, and a `ValueSpec`. The `ValueSpec` argument must be a `NonNull Object` ValueSpec, because it represents the selection set of the fragment.
-}
fragment :
    String
    -> TypeCondition
    -> ValueSpec NonNull ObjectType result vars
    -> Fragment result vars
fragment name typeCondition spec =
    Fragment
        { name = name
        , typeCondition = typeCondition
        , directives = []
        , spec = spec
        }


{-| Construct a `TypeCondition` from the name of an object, interface, or union type defined in a GraphQL schema.
-}
onType : String -> TypeCondition
onType =
    AST.TypeCondition


{-| Takes a list of field selections, all of whom must decode to the same Elm type, and returns a `ValueSpec` for an object that decodes to a list of corresponding `(key, value)` pairs. Especially useful when you want to dynamically query multiple variations of the same field using aliases. For example:

    userAvatarUrlField : String -> SelectionSpec Field String vars
    userAvatarUrlField name =
        aliasAs name <|
            field "user"
                [ ( "login", Arg.string name ) ]
                (extract (field "avatarUrl" [] string))

    userAvatarUrls : List String -> Document Query (List ( String, String )) vars
    userAvatarUrls names =
        queryDocument <|
            keyValuePairs (List.map userAvatarUrlField names)

If you used this code to construct a query document with `userAvatarUrls ["alice", "bob"]`, the resulting query would look like this:

    alice: user(login: "alice") {
      avatarUrl
    }
    bob: user(login: "bob") {
      avatarUrl
    }

...and a successful decoded result would look something like this:

    [ ( "alice", "https://cdn.example.com/alice.png" )
    , ( "bob", "https://cdn.example.com/bob.png" )
    ]

Note that field aliases must still conform to the GraphQL spec:

<https://facebook.github.io/graphql/#sec-Names>

This means that the above example would not be suitable to use when the usernames are supplied from user input. If the user supplies a name that is not a valid GraphQL alias, then the GraphQL server would return an error response. In a case like that where you are generating a query from user input, you will need to find some other way of generating the field aliases.

-}
keyValuePairs :
    List (SelectionSpec Field value vars)
    -> ValueSpec NonNull ObjectType (List ( String, value )) vars
keyValuePairs selections =
    List.foldr
        (\((SelectionSpec ast _ _ _) as selection) accSpec ->
            case ast of
                AST.Field fieldInfo ->
                    let
                        keyValueSpec =
                            extract selection
                                |> map (\value -> ( Util.responseKey fieldInfo, value ))
                    in
                    map2 (::) keyValueSpec accSpec

                _ ->
                    accSpec
        )
        (produce [])
        selections


{-| Just like `keyValuePairs`, but the decoded result is a `Dict String value` constructed by applying `Dict.fromList` to the decoded result of `keyValuePairs`.
-}
dict :
    List (SelectionSpec Field value vars)
    -> ValueSpec NonNull ObjectType (Dict String value) vars
dict =
    keyValuePairs >> map Dict.fromList


{-| Takes a constructor function for an Elm type you want to produce, and returns a `ValueSpec` for an object without any fields yet specified. This function is intended to start a pipeline of calls to the `with` function to add field and fragment selections to the `ValueSpec`. The order of arguments to the constructor function determines the order that the selections must be added. For example:

    type alias User =
        { name : String
        , isAdmin : Bool
        }

    userSummary : ValueSpec NonNull ObjectType User vars
    userSummary =
        object User
            |> with (field "name" [] string)
            |> with (field "isAdmin" [] bool)

The above `ValueSpec` produces a GraphQL selection set that looks like the following:

    {
      name
      isAdmin
    }

The same `ValueSpec` also provides a JSON decoder for decoding part of the response, equivalent to the following:

    Json.Decode.map2 User
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "isAdmin" Json.Decode.bool)

-}
object :
    (fieldValue -> a)
    -> ValueSpec NonNull ObjectType (fieldValue -> a) vars
object ctr =
    ValueSpec emptyObjectSpecifiedType (always (Decode.succeed ctr)) [] []


{-| Make a `ValueSpec` for an object with only a single `SelectionSpec`. In cases where you only need one selection from an object, this function lets you conveniently extract the decoded result of that selection directly into the parent result. For example, the following code uses `extract` to hoist the `id` field of each `Project` result into a list of `projectIDs` on the `User` record:

    type alias User =
        { name : String
        , projectIDs : List String
        }

    userSpec : ValueSpec NonNull ObjectType User vars
    userSpec =
        object User
            |> with (field "name" [])
            |> with
                (field "projects"
                    []
                    (list (extract (field "id" [] id)))
                )

This helps you avoid having extra levels of nesting that you don't need in your result types.

-}
extract :
    SelectionSpec selectionType result vars
    -> ValueSpec NonNull ObjectType result vars
extract (SelectionSpec selectionAST decoder vars fragments) =
    ValueSpec
        (SpecifiedType
            { nullability = nonNullFlag
            , coreType = ObjectType
            , join = always
            , selectionSet = AST.SelectionSet [ selectionAST ]
            }
        )
        (selectionDecoder selectionAST decoder)
        vars
        fragments


argumentsAST : List ( String, Arg.Value vars ) -> List ( String, AST.ArgumentValue )
argumentsAST =
    List.map (Tuple.mapSecond Arg.getAST)


{-| Constructs a `SelectionSpec` for a field that you can add to an object `ValueSpec` with the `with` function. Takes the name of the field, a list of arguments, and a `ValueSpec` for the field's value.
-}
field :
    String
    -> List ( String, Arg.Value vars )
    -> ValueSpec nullability coreType result vars
    -> SelectionSpec Field result vars
field name arguments (ValueSpec sourceType decoder fieldVars fragments) =
    let
        astFieldInfo =
            { alias = Nothing
            , name = name
            , arguments = argumentsAST arguments
            , directives = []
            , selectionSet = selectionSetFromSourceType sourceType
            }

        vars =
            VarUtil.mergeVariables (varsFromArguments arguments) fieldVars
    in
    SelectionSpec
        (AST.Field astFieldInfo)
        decoder
        vars
        fragments


updateInfoWithDirectives : List ( String, List ( String, Arg.Value vars ) ) -> { info | directives : List AST.Directive } -> { info | directives : List AST.Directive }
updateInfoWithDirectives directives info =
    { info | directives = List.map directiveAST directives }


selectionASTWithDirectives : List ( String, List ( String, Arg.Value vars ) ) -> AST.Selection -> AST.Selection
selectionASTWithDirectives directives selection =
    case selection of
        AST.Field info ->
            AST.Field (updateInfoWithDirectives directives info)

        AST.FragmentSpread info ->
            AST.FragmentSpread (updateInfoWithDirectives directives info)

        AST.InlineFragment info ->
            AST.InlineFragment (updateInfoWithDirectives directives info)


{-| Specify a list of directives to use with a GraphQL selection. Each directive takes the form of a tuple of `(directiveName, args)`. The returned `SelectionSpec`'s decoder wraps the result type in a `Maybe` to account for the fact that the server may omit the selection because of the directives.
-}
withDirectives :
    List ( String, List ( String, Arg.Value vars ) )
    -> SelectionSpec selectionType result vars
    -> SelectionSpec selectionType (Maybe result) vars
withDirectives directives (SelectionSpec ast decoder vars fragments) =
    SelectionSpec
        (selectionASTWithDirectives directives ast)
        (Decode.maybe << decoder)
        (VarUtil.mergeVariables (varsFromDirectives directives) vars)
        fragments


{-| Convert a `SelectionSpec` that decodes to a `Maybe` type into one that assumes the presence of a value and unwraps the `Just` wrapper for you. If `Nothing` is encountered, then the entire decoding process of the response will fail, so don't use this unless you are confident in your assumption! This function is most useful when you know that a fragment spread or inline fragment will successfully match on an object and don't want to deal with an unnecessary `Maybe` wrapper:

    type alias User =
        { id : String
        , name : String
        }

    userNameFragment : Fragment (Maybe String) vars
    userNameFragment =
        fragment "userNameFragment"
            (onType "User")
            (extract (field "name" [] string))

    userSpec : ValueSpec NonNull ObjectType User vars
    userSpec =
        object User
            |> with (field "id" [] id)
            |> with (assume (fragmentSpread userNameFragment))

As long as the above `userSpec` is only ever used for selection sets on the schema's `"User"` type, then the fragment should always be returned by the server and the `assume` will always succeed.

Depending on the semantics of the GraphQL schema you're working with, it may also be safe to use in some cases where fields are nullable in the schema but you know that in certain cases they are predictably non-null.

-}
assume :
    SelectionSpec selectionType (Maybe result) vars
    -> SelectionSpec selectionType result vars
assume (SelectionSpec ast decoder vars fragments) =
    SelectionSpec
        ast
        (decoder
            >> Decode.andThen
                (\maybeValue ->
                    case maybeValue of
                        Just value ->
                            Decode.succeed value

                        Nothing ->
                            Decode.fail "Expected a selection to be present in the response with `assume`, but found `Nothing`"
                )
        )
        vars
        fragments


{-| Give an alias to a field, overriding its response key. Useful when you need to ask for a field multiple times with different sets of arguments:

    type alias QueryRoot =
        { username1 : String
        , username2 : String
        }

    querySpec : ValueSpec NonNull ObjectType QueryRoot vars
    querySpec =
        object QueryRoot
            |> with
                (aliasAs "user1"
                    (field "user"
                        [ ( "id", Arg.id "1" ) ]
                        (extract (field "name" [] string))
                    )
                )
            |> with
                (aliasAs "user2"
                    (field "user"
                        [ ( "id", Arg.id "2" ) ]
                        (extract (field "name" [] string))
                    )
                )

-}
aliasAs :
    String
    -> SelectionSpec Field result vars
    -> SelectionSpec Field result vars
aliasAs responseKey ((SelectionSpec ast decoder vars fragments) as selection) =
    case ast of
        AST.Field info ->
            SelectionSpec
                (AST.Field { info | alias = Just responseKey })
                decoder
                vars
                fragments

        _ ->
            selection


{-| Constructs a `SelectionSpec` for a fragment spread that you can add to an object `ValueSpec` with the `with` function. Takes a `Fragment` and a list of optional directives. The directives are tuples whose first element is the name of the directive, and whose second element is a list of key-value tuples representing the directive arguments. Argument values are constructed using functions from [`GraphQL.Request.Builder.Value`](GraphQL-Request-Builder-Value).

The fragment decoder's result type is wrapped in a `Maybe` to account for fragments with type constraints that do not hold for all values of the parent `ValueSpec`. This means that normally the parent `ValueSpec`'s constructor function must accept a `Maybe` of the fragment result as its next argument:

    type alias User =
        { name : String
        , employeeInfo : Maybe EmployeeInfo
        }

    type alias EmployeeInfo =
        { employeeNumber : Int
        , title : String
        }

    employeeInfoFragment : Fragment EmployeeInfo vars
    employeeInfoFragment =
        fragment "employeeInfoFragment"
            (onType "Employee")
            (object EmployeeInfo
                |> with (field "employeeNumber" [] int)
                |> with (field "title" [] string)
            )

    userSpec : ValueSpec NonNull ObjectType User vars
    userSpec =
        object User
            |> with (field "name" [] string)
            |> with (fragmentSpread employeeInfoFragment [])

In cases where you know for sure that a fragment will successfully produce values in the response for a given `ValueSpec`, you can use the `assume` function to unwrap the `Maybe` for you.

Including the above `userSpec` anywhere in a `Document` results in the following fragment definition being included in the serialized output:

    fragment employeeInfoFragment on Employee {
      employeeNumber
      title
    }

Meanwhile, the selection set of `userSpec` itself would look like this wherever it's used:

    {
      name
      ...employeeInfoFragment
    }

-}
fragmentSpread :
    Fragment result vars
    -> SelectionSpec FragmentSpread (Maybe result) vars
fragmentSpread ((Fragment { name, spec }) as fragmentRecord) =
    let
        astFragmentSpreadInfo =
            { name = name
            , directives = []
            }

        (ValueSpec _ decoder _ nestedFragments) =
            spec
    in
    SelectionSpec
        (AST.FragmentSpread astFragmentSpreadInfo)
        (Decode.maybe << decoder)
        (fragmentVariables fragmentRecord)
        (mergeFragments [ fragmentAST fragmentRecord ] nestedFragments)


{-| Constructs a `SelectionSpec` for an object with a single inline fragment. Takes an optional `TypeCondition`, a list of optional directives, and a `ValueSpec` representing the selection set of the inline fragment. The directives are tuples whose first element is the name of the directive, and whose second element is a list of key-value tuples representing the directive arguments. Argument values are constructed using functions from [`GraphQL.Request.Builder.Value`](GraphQL-Request-Builder-Value).

The result type of the inline fragment's `SelectionSpec` is wrapped in a `Maybe` to account for type constraints that do not hold for all values of the parent `ValueSpec`. This means that the parent `ValueSpec`'s constructor function must accept a `Maybe` of the fragment result as its next argument:

    type alias User =
        { name : String
        , employeeInfo : Maybe EmployeeInfo
        }

    type alias EmployeeInfo =
        { employeeNumber : Int
        , title : String
        }

    userSpec : ValueSpec NonNull ObjectType User vars
    userSpec =
        object User
            |> with (field "name" [] string)
            |> with
                (inlineFragment (Just (onType "Employee"))
                    []
                    (object EmployeeInfo
                        |> with (field "employeeNumber" [] int)
                        |> with (field "title" [] string)
                    )
                )

The selection set of the above `userSpec` would look like the following wherever it's used:

    {
      name
      ... on Employee {
        employeeNumber
        title
      }
    }

-}
inlineFragment :
    Maybe TypeCondition
    -> ValueSpec NonNull ObjectType result vars
    -> SelectionSpec InlineFragment (Maybe result) vars
inlineFragment maybeTypeCondition spec =
    let
        (ValueSpec sourceType decoder vars fragments) =
            spec

        astInlineFragmentInfo =
            { typeCondition = maybeTypeCondition
            , directives = []
            , selectionSet = selectionSetFromSourceType sourceType
            }
    in
    SelectionSpec
        (AST.InlineFragment astInlineFragmentInfo)
        (Decode.maybe << decoder)
        vars
        fragments


varsFromArguments : List ( String, Arg.Value vars ) -> List (Variable vars)
varsFromArguments arguments =
    List.foldr
        (Tuple.second
            >> Arg.getVariables
            >> VarUtil.mergeVariables
        )
        []
        arguments


{-| A `ValueSpec` for the GraphQL `Int` type that decodes to an Elm `Int`.
-}
int : ValueSpec NonNull IntType Int vars
int =
    primitiveSpec IntType Decode.int


{-| A `ValueSpec` for the GraphQL `Float` type that decodes to an Elm `Float`.
-}
float : ValueSpec NonNull FloatType Float vars
float =
    primitiveSpec FloatType Decode.float


{-| A `ValueSpec` for the GraphQL `String` type that decodes to an Elm `String`.
-}
string : ValueSpec NonNull StringType String vars
string =
    primitiveSpec StringType Decode.string


{-| A `ValueSpec` for the GraphQL `Boolean` type that decodes to an Elm `Bool`.
-}
bool : ValueSpec NonNull BooleanType Bool vars
bool =
    primitiveSpec BooleanType Decode.bool


{-| A `ValueSpec` for the GraphQL `ID` type that decodes to an Elm `String`.
-}
id : ValueSpec NonNull IdType String vars
id =
    primitiveSpec IdType Decode.string


{-| Constructs a `ValueSpec` for a GraphQL Enum type. Takes a list of string-result pairs to map Enum values to `result` values. For example:

    type AccessLevel
        = AdminAccess
        | MemberAccess

    userAccessLevel : ValueSpec NonNull EnumType AccessLevel vars
    userAccessLevel =
        enum
            [ ( "ADMIN", AdminAccess )
            , ( "MEMBER", MemberAccess )
            ]

-}
enum : List ( String, result ) -> ValueSpec NonNull EnumType result vars
enum =
    enumWithFallback
        (\label ->
            Decode.fail ("Unexpected enum value " ++ Encode.encode 0 (Encode.string label))
        )


{-| Constructs a `ValueSpec` for a GraphQL Enum type. Works the same as `enum`, but takes a default function to produce a `result` value from any Enum value not specified in the list of known Enum values. This is useful if you expect a schema to add more possible values to an Enum type in the future and don't want to bail out on the decoding process every time you encounter something you haven't seen before:

    type AccessLevel
        = AdminAccess
        | MemberAccess
        | UnknownAccess String

    userAccessLevel : ValueSpec NonNull EnumType AccessLevel vars
    userAccessLevel =
        enumWithDefault UnknownAccess
            [ ( "ADMIN", AdminAccess )
            , ( "MEMBER", MemberAccess )
            ]

-}
enumWithDefault :
    (String -> result)
    -> List ( String, result )
    -> ValueSpec NonNull EnumType result vars
enumWithDefault ctr =
    enumWithFallback
        (\label ->
            Decode.succeed (ctr label)
        )


{-| Create a `ValueSpec` for a custom scalar type defined in the GraphQL schema you're interacting with. The `customTypeMarker` is an Elm type that you define specifically for this custom scalar `ValueSpec` you're making. The type should have a single constructor that takes no arguments, and both should have the CamelCase name of the custom scalar type in the GraphQL schema, plus a `"Type"` suffix. For example, if your GraphQL schema has a `Time` type, you should define your `customTypeMarker` like the following, in some module of your choosing:

    type TimeType
        = TimeType

The type marker is used by this package to help make sure `ValueSpec`s are only combined in valid ways. In the future, it may be used to help you validate that your queries and mutations against a target schema in your unit tests.

Once you have `TimeType` to use as a type marker, you can define a `ValueSpec` for the `Time` GraphQL type by supplying `TimeType` as a runtime argument to the function along with a JSON decoder that works with values of the type. For example, you might decide to use the `elm-iso8601` package for parsing and define the `ValueSpec` like so:

    import ISO8601
    import Json.Decode as Decode

    type TimeType
        = TimeType

    time : ValueSpec NonNull TimeType ISO8601.Time vars
    time =
        Decode.string
            |> Decode.andThen
                (\timeString ->
                    case ISO8601.fromString timeString of
                        Ok time ->
                            Decode.succeed time

                        Err errorMessage ->
                            Decode.fail errorMessage
                )
            |> customScalar TimeType

-}
customScalar :
    customTypeMarker
    -> Decoder result
    -> ValueSpec NonNull customTypeMarker result vars
customScalar customTypeMarker decoder =
    primitiveSpec customTypeMarker decoder


enumWithFallback :
    (String -> Decoder result)
    -> List ( String, result )
    -> ValueSpec NonNull EnumType result vars
enumWithFallback fallbackDecoder labelledValues =
    let
        decoderFromLabel =
            decoderFromEnumLabel fallbackDecoder labelledValues

        decoder =
            Decode.string
                |> Decode.andThen decoderFromLabel

        labels =
            List.map Tuple.first labelledValues
    in
    ValueSpec
        (SpecifiedType
            { nullability = nonNullFlag
            , coreType = EnumType labels
            , join = enumJoin
            , selectionSet = emptySelectionSet
            }
        )
        (always decoder)
        []
        []


decoderFromEnumLabel :
    (String -> Decoder a)
    -> List ( String, a )
    -> String
    -> Decoder a
decoderFromEnumLabel fallbackDecoder labelledValues =
    let
        valueFromLabel =
            \key -> Dict.get key (Dict.fromList labelledValues)

        decoder enumString =
            case valueFromLabel enumString of
                Just value ->
                    Decode.succeed value

                Nothing ->
                    fallbackDecoder enumString
    in
    decoder


{-| Constructs a `ValueSpec` for a GraphQL List type. Takes any kind of `ValueSpec` to use for the items of the list, and returns a `ValueSpec` that decodes into an Elm `List`.
-}
list :
    ValueSpec itemNullability itemType result vars
    -> ValueSpec NonNull (ListType itemNullability itemType) (List result) vars
list (ValueSpec itemType decoder vars fragments) =
    ValueSpec
        (SpecifiedType
            { nullability = nonNullFlag
            , coreType = ListType itemType
            , join = listJoin
            , selectionSet = selectionSetFromSourceType itemType
            }
        )
        (Decode.list << decoder)
        vars
        fragments


{-| Transforms a `NonNull` `ValueSpec` into one that allows `null` values, using a `Maybe` of the original `ValueSpec`'s `result` type to represent the nullability in the decoded Elm value.

Note that the default `nullability` of a `ValueSpec` in this module is `NonNull`. This is the opposite of the situation in the GraphQL schema language, whose types must be annotated with the Non-Null (`!`) modifier in order to specify that their values will never be `null`.

-}
nullable :
    ValueSpec NonNull coreType result vars
    -> ValueSpec Nullable coreType (Maybe result) vars
nullable (ValueSpec sourceType decoder vars fragments) =
    case sourceType of
        SpecifiedType typeInfo ->
            ValueSpec
                (SpecifiedType (SpecifiedTypeInfo nullableFlag typeInfo.coreType typeInfo.join typeInfo.selectionSet))
                (Decode.nullable << decoder)
                vars
                fragments

        AnyType ->
            ValueSpec AnyType (Decode.nullable << decoder) vars fragments


emptyObjectSpecifiedType : SourceType NonNull ObjectType
emptyObjectSpecifiedType =
    SpecifiedType
        { nullability = nonNullFlag
        , coreType = ObjectType
        , join = always
        , selectionSet = emptySelectionSet
        }


{-| Construct a `ValueSpec` that always decodes to the given `result`, without using anything from the response value.
-}
produce : result -> ValueSpec nullability coreType result vars
produce x =
    ValueSpec AnyType (always (Decode.succeed x)) [] []


{-| Transform the decoded value of a `ValueSpec` with the given function, just like `Json.Decode.map`. Here it is used to wrap an `id` field in a custom type:

    type UserId
        = UserId String

    type alias User =
        { id : UserId
        , name : String
        }

    user : ValueSpec NonNull ObjectType User vars
    user =
        object User
            |> with (field "id" [] (map UserId id))
            |> with (field "name" [] string)

Here's an example of using `map` with `nullable` to implement a function that can provide a default value for nullable fields:

    nullableWithDefault :
        a
        -> ValueSpec NonNull coreType a vars
        -> ValueSpec Nullable coreType a vars
    nullableWithDefault default spec =
        map (Maybe.withDefault default) (nullable spec)

-}
map : (a -> b) -> ValueSpec nullability coreType a vars -> ValueSpec nullability coreType b vars
map f (ValueSpec sourceType decoder vars fragments) =
    ValueSpec sourceType (decoder >> Decode.map f) vars fragments


map2 :
    (a -> b -> c)
    -> ValueSpec nullability coreType a vars
    -> ValueSpec nullability coreType b vars
    -> ValueSpec nullability coreType c vars
map2 f (ValueSpec sourceTypeA decoderA varsA fragmentsA) (ValueSpec sourceTypeB decoderB varsB fragmentsB) =
    let
        joinedSourceType =
            join sourceTypeA sourceTypeB

        joinedDecoder selectionSet =
            Decode.map2 f (decoderA selectionSet) (decoderB selectionSet)

        mergedVariables =
            VarUtil.mergeVariables varsA varsB

        mergedFragments =
            mergeFragments fragmentsA fragmentsB
    in
    ValueSpec joinedSourceType joinedDecoder mergedVariables mergedFragments


{-| Use this function to add `SelectionSpec`s to an object `ValueSpec` pipeline:

    type alias User =
        { name : String
        , adminAccess : Bool
        }

    userSpec : ValueSpec NonNull ObjectType User vars
    userSpec =
        object User
            |> with (field "name" [] string)
            |> with (field "adminAccess" [] bool)

-}
with :
    SelectionSpec selectionType a vars
    -> ValueSpec NonNull ObjectType (a -> b) vars
    -> ValueSpec NonNull ObjectType b vars
with selection objectSpec =
    map2 (<|) objectSpec (extract selection)


{-| Adds a hardcoded local constant value to an object `ValueSpec` pipeline. This can be useful for initializing records with default local state values:

    type alias Item =
        { name : String
        , selected : Bool
        }

    itemSpec : ValueSpec NonNull ObjectType Item vars
    itemSpec =
        object Item
            |> with (field "name" [] string)
            |> withLocalConstant False

Any `Item` record decoded by `itemSpec` would then have its `selected` field initialized to `False`. Adding a local constant in this way has no effect on the corresponding GraphQL selection set that is sent to the server — `itemSpec`'s selection set would simply be `{ name }`.

-}
withLocalConstant :
    a
    -> ValueSpec NonNull ObjectType (a -> b) vars
    -> ValueSpec NonNull ObjectType b vars
withLocalConstant x objectSpec =
    map2 (<|) objectSpec (produce x)


enumJoin : EnumType -> EnumType -> EnumType
enumJoin (EnumType labelsA) (EnumType labelsB) =
    Set.fromList labelsA
        |> Set.intersect (Set.fromList labelsB)
        |> Set.toList
        |> EnumType


listJoin :
    ListType itemNullability itemType
    -> ListType itemNullability itemType
    -> ListType itemNullability itemType
listJoin (ListType itemSourceTypeA) (ListType itemSourceTypeB) =
    ListType (join itemSourceTypeA itemSourceTypeB)


mergeSelectionSets : AST.SelectionSet -> AST.SelectionSet -> AST.SelectionSet
mergeSelectionSets (AST.SelectionSet selectionsA) (AST.SelectionSet selectionsB) =
    AST.SelectionSet (selectionsA ++ selectionsB)


join : SourceType nullability coreType -> SourceType nullability coreType -> SourceType nullability coreType
join a b =
    case ( a, b ) of
        ( SpecifiedType typeInfoA, SpecifiedType typeInfoB ) ->
            SpecifiedType
                { typeInfoA
                    | coreType = typeInfoA.join typeInfoA.coreType typeInfoB.coreType
                    , selectionSet = mergeSelectionSets typeInfoA.selectionSet typeInfoB.selectionSet
                }

        ( AnyType, _ ) ->
            b

        ( _, AnyType ) ->
            a


selectionSetFromSourceType : SourceType nullability coreType -> AST.SelectionSet
selectionSetFromSourceType sourceType =
    case sourceType of
        SpecifiedType { selectionSet } ->
            selectionSet

        AnyType ->
            emptySelectionSet


selectionSetFromSpec : ValueSpec nullability coreType result vars -> AST.SelectionSet
selectionSetFromSpec (ValueSpec sourceType _ _ _) =
    selectionSetFromSourceType sourceType


emptySelectionSet : AST.SelectionSet
emptySelectionSet =
    AST.SelectionSet []


primitiveSpec : coreType -> Decoder result -> ValueSpec NonNull coreType result vars
primitiveSpec coreType decoder =
    ValueSpec
        (SpecifiedType
            { nullability = nonNullFlag
            , coreType = coreType
            , join = always
            , selectionSet = emptySelectionSet
            }
        )
        (always decoder)
        []
        []


variableDefinitionsAST :
    ValueSpec nullability coreType result vars
    -> List AST.VariableDefinition
variableDefinitionsAST (ValueSpec _ _ vars _) =
    List.map Variable.toDefinitionAST vars


directiveAST :
    ( String, List ( String, Arg.Value a ) )
    -> AST.Directive
directiveAST ( name, arguments ) =
    AST.Directive
        { name = name
        , arguments = argumentsAST arguments
        }


operationTypeAST : OperationType operationType -> AST.OperationType
operationTypeAST operationType =
    case operationType of
        QueryOperationType ->
            AST.Query

        MutationOperationType ->
            AST.Mutation


operationAST : Operation operationType result vars -> AST.OperationDefinitionInfo
operationAST (Operation { operationType, name, directives, spec }) =
    { operationType = operationTypeAST operationType
    , name = name
    , variableDefinitions = variableDefinitionsAST spec
    , directives = List.map directiveAST directives
    , selectionSet = selectionSetFromSpec spec
    }


fragmentAST : Fragment result vars -> AST.FragmentDefinitionInfo
fragmentAST (Fragment { name, typeCondition, directives, spec }) =
    { name = name
    , typeCondition = typeCondition
    , directives = List.map directiveAST directives
    , selectionSet = selectionSetFromSpec spec
    }


varsFromDirectives : List ( String, List ( String, Arg.Value vars ) ) -> List (Variable vars)
varsFromDirectives =
    List.concatMap (Tuple.second >> varsFromArguments)


fragmentVariables : Fragment result vars -> List (Variable vars)
fragmentVariables (Fragment { directives, spec }) =
    let
        directiveVariables =
            varsFromDirectives directives

        (ValueSpec _ _ specVariables _) =
            spec
    in
    VarUtil.mergeVariables directiveVariables specVariables


documentAST : Document operationType result vars -> AST.Document
documentAST (Document { ast }) =
    ast


documentString : Document operationType result vars -> String
documentString (Document { serialized }) =
    serialized


documentResponseDecoder :
    Document operationType result vars
    -> Decoder result
documentResponseDecoder (Document { operation }) =
    let
        (Operation { spec }) =
            operation
    in
    specDecoder spec


documentVariables : Document operationType result vars -> List (Variable vars)
documentVariables (Document { operation }) =
    let
        (Operation { spec }) =
            operation

        (ValueSpec _ _ vars _) =
            spec
    in
    vars


specDecoder : ValueSpec nullability coreType result vars -> Decoder result
specDecoder (ValueSpec sourceType decoderFromSelectionSet _ _) =
    sourceType
        |> selectionSetFromSourceType
        |> decoderFromSelectionSet


mergeFragments : List AST.FragmentDefinitionInfo -> List AST.FragmentDefinitionInfo -> List AST.FragmentDefinitionInfo
mergeFragments fragmentsA fragmentsB =
    fragmentsA
        ++ (fragmentsB
                |> List.filter
                    (\fragmentItem -> not (List.any ((==) fragmentItem) fragmentsA))
           )
