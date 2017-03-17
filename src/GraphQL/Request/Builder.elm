module GraphQL.Request.Builder
    exposing
        ( Request
        , Document
        , Query
        , Mutation
        , Fragment
        , Spec
        , Nullable
        , NonNull
        , IntType
        , FloatType
        , StringType
        , BooleanType
        , IdType
        , EnumType
        , ListType
        , ObjectType
        , FieldOption
        , TypeCondition
        , request
        , requestBody
        , jsonVariableValues
        , responseDataDecoder
        , queryDocument
        , mutationDocument
        , fragment
        , onType
        , int
        , float
        , string
        , bool
        , id
        , enum
        , enumWithDefault
        , list
        , nullable
        , object
        , withField
        , field
        , alias
        , args
        , directive
        , withFragment
        , fragmentSpread
        , withInlineFragment
        , inlineFragment
        , produce
        , map
        , map2
        , map3
        , map4
        , map5
        , map6
        , map7
        , map8
        , with
        )

{-| This module provides an interface for building up GraphQL requests in a way that gives you everything you need to safely and conveniently integrate them with your Elm program:

* GraphQL variables are automatically converted from corresponding Elm types, so the compiler will let you know if there's a mismatch between the variables used in a document and the values you provide when you send the request.
* Responses from the server are decoded using a `Json.Decode.Decoder` value that is built up as you build each part of the request document.

In order to use arguments and variables in your requests, you will need to use functions from the [`GraphQL.Request.Builder.Value`](GraphQL-Request-Builder-Value) and [`GraphQL.Request.Builder.Variable`](GraphQL-Request-Builder-Variable) modules. To send your requests over HTTP, see the [`GraphQL.Client.Http`](GraphQL-Client-Http) module.


# Specs

@docs Spec, NonNull, Nullable, IntType, FloatType, StringType, BooleanType, IdType, EnumType, ListType, ObjectType

## Objects

@docs object

### Fields

@docs withField, field, FieldOption, alias, args, directive

### Fragments

@docs Fragment, fragment, TypeCondition, onType, withFragment, fragmentSpread, withInlineFragment, inlineFragment

## Scalars

@docs int, float, string, bool, id, enum, enumWithDefault

## Nullability

@docs nullable

## Lists

@docs list

## Spec composition

@docs produce, map, with, map2, map3, map4, map5, map6, map7, map8

# Documents

@docs Document, Query, queryDocument, Mutation, mutationDocument

# Requests

@docs Request, request, requestBody, jsonVariableValues, responseDataDecoder

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import GraphQL.Request.Document.AST as AST
import GraphQL.Request.Document.AST.Serialize as Serialize
import GraphQL.Request.Document.AST.Value.Json.Encode as ValueEncode
import GraphQL.Request.Document.AST.Util as Util
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Variable exposing (Variable)
import Dict exposing (Dict)
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


{-| A `Document` represents a single-operation GraphQL request document, along with the information necessary to encode variable values used in the document and to decode successful responses from the server. The `variableSource` parameter is the type of value that must be provided when constructing a `Request` in order for the document's variable values to be obtained. The `operationType` and `result` parameters are the same as in the `Request` type.
-}
type Document operationType variableSource result
    = Document
        { operation : Operation operationType variableSource result
        , ast : AST.Document
        , serialized : String
        }


type Operation operationType variableSource result
    = Operation
        { operationType : OperationType operationType
        , name : Maybe String
        , directives : List ( String, List ( String, Arg.Value variableSource ) )
        , spec : Spec NonNull ObjectType variableSource result
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


{-| A fragment definition. The `variableSource` parameter specifies the type of the Elm value required to supply any variables used anywhere within the fragment. The `result` parameter is the type of the Elm value obtained from the `Fragment`'s JSON decoder.
-}
type Fragment variableSource result
    = Fragment
        { name : String
        , typeCondition : TypeCondition
        , directives : List ( String, List ( String, Arg.Value variableSource ) )
        , spec : Spec NonNull ObjectType variableSource result
        }


{-| A `Spec` is a structured way of describing a value that you want back from a GraphQL server, and it is the fundamental building block of the request builder interface provided by this module. It corresponds loosely with the GraphQL concept of the "selection set", but it is used for scalar values as well as object values, and holds more information about their expected types.

The `nullability` and `coreType` parameters are used by various functions in this module to ensure consistency when combining `Spec` values. As such, they will probably only become relevant to you when reading error messages from the compiler, at which point they will hopefully make the situation easier to understand.

The `variableSource` parameter specifies the type of the Elm value required to supply any variables used anywhere within the `Spec`.

The `result` parameter specifies the type produced by the JSON decoder of the `Spec`.
-}
type Spec nullability coreType variableSource result
    = Spec (SourceType nullability coreType) (AST.SelectionSet -> Decoder result) (List (Variable variableSource)) (List AST.FragmentDefinitionInfo)


type SourceType nullability coreType
    = SpecifiedType (SpecifiedTypeInfo nullability coreType)
    | AnyType


type alias SpecifiedTypeInfo nullability coreType =
    { nullability : Nullability nullability
    , coreType : coreType
    , join : coreType -> coreType -> coreType
    , selectionSet : AST.SelectionSet
    }


{-| Indicates that a `Spec` describes GraphQL values that may be `null`.
-}
type Nullable
    = Nullable


{-| Indicates that a `Spec` describes GraphQL values that may not be `null`. Unlike in the GraphQL schema language, `NonNull` is the default in this library.
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


{-| Indicates that a `Spec` describes GraphQL `Int` values.
-}
type IntType
    = IntType


{-| Indicates that a `Spec` describes GraphQL `Float` values.
-}
type FloatType
    = FloatType


{-| Indicates that a `Spec` describes GraphQL `String` values.
-}
type StringType
    = StringType


{-| Indicates that a `Spec` describes GraphQL `Boolean` values.
-}
type BooleanType
    = BooleanType


{-| Indicates that a `Spec` describes GraphQL `ID` values.
-}
type IdType
    = IdType


{-| Indicates that a `Spec` describes values of some GraphQL Enum type.
-}
type EnumType
    = EnumType (List String)


{-| Indicates that a `Spec` describes values of some GraphQL List type.
-}
type ListType itemNullability itemCoreType
    = ListType (SourceType itemNullability itemCoreType)


{-| Indicates that a `Spec` describes values of some GraphQL Object type.
-}
type ObjectType
    = ObjectType


{-| An option for a field, returned by the `alias`, `args`, and `directive` functions.
-}
type FieldOption variableSource
    = FieldAlias String
    | FieldArgs (List ( String, Arg.Value variableSource ))
    | FieldDirective String (List ( String, Arg.Value variableSource ))


{-| Turn a `Document` into a `Request` that can be sent to a server, by supplying a `variableSource` value that is used to obtain values for any variables used in the `Document`. If the `Document` does not use any variables, then you can pass in `()` or any other value as the `variableSource` and it will be ignored.
-}
request :
    variableSource
    -> Document operationType variableSource result
    -> Request operationType result
request variableSource ((Document { operation, ast, serialized }) as document) =
    Request
        { documentAST = ast
        , documentString = serialized
        , variableValues = (documentVariables document |> Variable.extractValuesFrom variableSource)
        , responseDataDecoder = documentResponseDecoder document
        }


{-| Get the serialized document body of a `Request`.
-}
requestBody : Request operationType result -> String
requestBody (Request { documentString }) =
    documentString


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
responseDataDecoder (Request { responseDataDecoder }) =
    responseDataDecoder


fragmentDefinitionsFromOperation : Operation operationType variableSource result -> List AST.FragmentDefinitionInfo
fragmentDefinitionsFromOperation (Operation { spec }) =
    let
        (Spec _ _ _ fragments) =
            spec
    in
        fragments


document : Operation operationType variableSource result -> Document operationType variableSource result
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


{-| Take a `Spec` and return a `Document` for a single query operation. The argument must be a `NonNull Object` Spec, because it represents the root-level selection set of the query operation.
-}
queryDocument :
    Spec NonNull ObjectType variableSource result
    -> Document Query variableSource result
queryDocument spec =
    document
        (Operation
            { operationType = queryOperationType
            , name = Nothing
            , directives = []
            , spec = spec
            }
        )


queryOperationType : OperationType Query
queryOperationType =
    QueryOperationType


{-| Take a `Spec` and return a `Document` for a single mutation operation. The argument must be a `NonNull Object` Spec, because it represents the root-level selection set of the mutation operation.
-}
mutationDocument :
    Spec NonNull ObjectType variableSource result
    -> Document Mutation variableSource result
mutationDocument spec =
    document
        (Operation
            { operationType = mutationOperationType
            , name = Nothing
            , directives = []
            , spec = spec
            }
        )


mutationOperationType : OperationType Mutation
mutationOperationType =
    MutationOperationType


{-| Construct a `Fragment` by providing a name, a `TypeCondition`, and a `Spec`. The `Spec` argument must be a `NonNull Object` Spec, because it represents the selection set of the fragment.
-}
fragment :
    String
    -> TypeCondition
    -> Spec NonNull ObjectType variableSource result
    -> Fragment variableSource result
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


{-| Takes a constructor function for an Elm type you want to produce, and returns a `Spec` for an object without any fields yet specified. This function is intended to be used with other pipeline-friendly functions like `withField`, `withFragment`, and `withInlineFragment` to build up `Spec` values that correspond to object selection sets. When a successful response is decoded, the decoded value of each selection in the pipeline gets threaded in the same order as arguments to the given constructor function to produce the final result. For example:

    type alias User =
        { name : String
        , email : String
        }

    userSummary : Spec NonNull ObjectType variableSource User
    userSummary =
        object User
            |> withField "name" [] string
            |> withField "email" [] string

The above `Spec` produces a GraphQL selection set that looks like the following:

    {
      name
      email
    }

The above `Spec` also provides a JSON decoder for decoding the corresponding part of the response, equivalent to the following:

    Json.Decode.map2 User
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "email" Json.Decode.string)
-}
object :
    (fieldValue -> a)
    -> Spec NonNull ObjectType variableSource (fieldValue -> a)
object ctr =
    Spec emptyObjectSpecifiedType (always (Decode.succeed ctr)) [] []


{-| Adds a field to an object `Spec` pipeline. The first argument is the name of the field. The second argument is a list of `FieldOption` values, allowing you to optionally specify an alias, arguments, and/or directives for the field. The third argument is a `Spec` for the value of the field, and the final argument (usually threaded via `|>`) is the in-progress `Spec` corresponding to the parent object.
-}
withField :
    String
    -> List (FieldOption variableSource)
    -> Spec nullability coreType variableSource a
    -> Spec NonNull ObjectType variableSource (a -> b)
    -> Spec NonNull ObjectType variableSource b
withField name fieldOptions spec fSpec =
    fSpec
        |> with (field name fieldOptions spec)


{-| Constructs a `Spec` for an object with a single field. The arguments are the same as those for `withField`, except that the final `Spec` argument representing the parent object is omitted.
-}
field :
    String
    -> List (FieldOption variableSource)
    -> Spec nullability coreType variableSource result
    -> Spec NonNull ObjectType variableSource result
field name fieldOptions (Spec sourceType fieldDecoder fieldVars fragments) =
    let
        astFieldInfo =
            fieldOptions
                |> List.foldl applyFieldOption
                    { alias = Nothing
                    , name = name
                    , arguments = []
                    , directives = []
                    , selectionSet = selectionSetFromSourceType sourceType
                    }

        selectionSet =
            AST.SelectionSet [ AST.Field astFieldInfo ]

        decoder selectionSet =
            let
                fieldInSelectionSet =
                    astFieldInfo

                responseKey =
                    Util.responseKey fieldInSelectionSet
            in
                Decode.field responseKey (fieldDecoder fieldInSelectionSet.selectionSet)

        vars =
            mergeVariables (List.concatMap varsFromFieldOption fieldOptions) fieldVars
    in
        Spec
            (SpecifiedType
                { nullability = nonNullFlag
                , coreType = ObjectType
                , join = always
                , selectionSet = selectionSet
                }
            )
            decoder
            vars
            fragments


{-| Specify an alias for a field, overriding the property key used for the field in the JSON response. Returns a `FieldOption` to be passed to `withField` or `field`.
-}
alias : String -> FieldOption variableSource
alias =
    FieldAlias


{-| Specify arguments for a field in the form of key-value pairs. Values are constructed using functions from [`GraphQL.Request.Builder.Value`](GraphQL-Request-Builder-Value). Returns a `FieldOption` to be passed to `withField` or `field`.
-}
args : List ( String, Arg.Value variableSource ) -> FieldOption variableSource
args =
    FieldArgs


{-| Specify a directive for a field by passing the name of the directive (e.g. "skip" or "include") plus a list of arguments in the form of key-value pairs. Argument values are constructed using functions from [`GraphQL.Request.Builder.Value`](GraphQL-Request-Builder-Value). Returns a `FieldOption` to be passed to `withField` or `field`.
-}
directive : String -> List ( String, Arg.Value variableSource ) -> FieldOption variableSource
directive =
    FieldDirective


{-| Adds a fragment spread to an object `Spec` pipeline. Takes a `Fragment`, a list of optional directives, and a `Spec` of the parent object (intended to be threaded with `|>`). The directives are tuples whose first element is the name of the directive, and whose second element is a list of key-value tuples representing the directive arguments. Argument values are constructed using functions from [`GraphQL.Request.Builder.Value`](GraphQL-Request-Builder-Value).

The fragment decoder's result type is wrapped in a `Maybe` to account for fragments with type constraints that do not hold for all values of the parent `Spec`. This means that the parent `Spec`'s constructor function must accept a `Maybe` of the fragment result as its next argument:

    type alias User =
        { name : String
        , employeeInfo : Maybe EmployeeInfo
        }

    type alias EmployeeInfo =
        { employeeNumber : Int
        , title : String
        }

    employeeInfoFragment : Fragment variableSource EmployeeInfo
    employeeInfoFragment =
        fragment "employeeInfoFragment"
            (onType "Employee")
            (object EmployeeInfo
                |> withField "employeeNumber" [] int
                |> withField "title" [] string
            )

    userSpec : Spec NonNull ObjectType variableSource User
    userSpec =
        object User
            |> withField "name" [] sring
            |> withFragment employeeInfoFragment []

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
withFragment :
    Fragment variableSource a
    -> List ( String, List ( String, Arg.Value variableSource ) )
    -> Spec NonNull ObjectType variableSource (Maybe a -> b)
    -> Spec NonNull ObjectType variableSource b
withFragment fragment directives fSpec =
    fSpec
        |> with (fragmentSpread fragment directives)


{-| Constructs a `Spec` for an object with a single fragment spread. The arguments are the same as those for `withFragment`, except that the final `Spec` argument representing the parent object is omitted.
-}
fragmentSpread :
    Fragment variableSource result
    -> List ( String, List ( String, Arg.Value variableSource ) )
    -> Spec NonNull ObjectType variableSource (Maybe result)
fragmentSpread ((Fragment { name, spec }) as fragment) directives =
    let
        astFragmentSpreadInfo =
            { name = name
            , directives = List.map directiveAST directives
            }

        selectionSet =
            AST.SelectionSet [ AST.FragmentSpread astFragmentSpreadInfo ]

        (Spec _ decoder _ nestedFragments) =
            spec
    in
        Spec
            (SpecifiedType
                { nullability = nonNullFlag
                , coreType = ObjectType
                , join = always
                , selectionSet = selectionSet
                }
            )
            (Decode.maybe << decoder)
            (mergeVariables (varsFromDirectives directives) (fragmentVariables fragment))
            (mergeFragments [ fragmentAST fragment ] nestedFragments)


{-| Adds an inline fragment to an object `Spec` pipeline. Takes an optional `TypeCondition`, a list of optional directives, a `Spec` representing the selection set of the inline fragment, and another `Spec` for the parent object being constructed (intended to be threaded in with `|>`). The directives are tuples whose first element is the name of the directive, and whose second element is a list of key-value tuples representing the directive arguments. Argument values are constructed using functions from [`GraphQL.Request.Builder.Value`](GraphQL-Request-Builder-Value).

The result type of the inline fragment's `Spec` is wrapped in a `Maybe` to account for type constraints that do not hold for all values of the parent `Spec`. This means that the parent `Spec`'s constructor function must accept a `Maybe` of the fragment result as its next argument:

    type alias User =
        { name : String
        , employeeInfo : Maybe EmployeeInfo
        }

    type alias EmployeeInfo =
        { employeeNumber : Int
        , title : String
        }

    userSpec : Spec NonNull ObjectType variableSource User
    userSpec =
        object User
            |> withField "name" [] sring
            |> withInlineFragment
                (Just (onType "Employee"))
                []
                (object EmployeeInfo
                    |> withField "employeeNumber" [] int
                    |> withField "title" [] string
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
withInlineFragment :
    Maybe TypeCondition
    -> List ( String, List ( String, Arg.Value variableSource ) )
    -> Spec NonNull ObjectType variableSource a
    -> Spec NonNull ObjectType variableSource (Maybe a -> b)
    -> Spec NonNull ObjectType variableSource b
withInlineFragment maybeTypeCondition directives spec fSpec =
    fSpec
        |> with (inlineFragment maybeTypeCondition directives spec)


{-| Constructs a `Spec` for an object with a single inline fragment. The arguments are the same as those for `withInlineFragment`, except that the final `Spec` argument representing the parent object is omitted.
-}
inlineFragment :
    Maybe TypeCondition
    -> List ( String, List ( String, Arg.Value variableSource ) )
    -> Spec NonNull ObjectType variableSource result
    -> Spec NonNull ObjectType variableSource (Maybe result)
inlineFragment maybeTypeCondition directives spec =
    let
        (Spec sourceType decoder vars fragments) =
            spec

        astInlineFragmentInfo =
            { typeCondition = maybeTypeCondition
            , directives = List.map directiveAST directives
            , selectionSet = selectionSetFromSourceType sourceType
            }

        selectionSet =
            AST.SelectionSet [ AST.InlineFragment astInlineFragmentInfo ]
    in
        Spec
            (SpecifiedType
                { nullability = nonNullFlag
                , coreType = ObjectType
                , join = always
                , selectionSet = selectionSet
                }
            )
            (Decode.maybe << decoder)
            (mergeVariables (varsFromDirectives directives) vars)
            fragments


varsFromArguments : List ( String, Arg.Value variableSource ) -> List (Variable variableSource)
varsFromArguments arguments =
    List.concatMap (Arg.getVariables << Tuple.second) arguments


varsFromFieldOption : FieldOption variableSource -> List (Variable variableSource)
varsFromFieldOption fieldOption =
    case fieldOption of
        FieldAlias _ ->
            []

        FieldArgs arguments ->
            varsFromArguments arguments

        FieldDirective _ arguments ->
            varsFromArguments arguments


{-| A `Spec` for the GraphQL `Int` type that decodes to an Elm `Int`.
-}
int : Spec NonNull IntType variableSource Int
int =
    primitiveSpec IntType Decode.int


{-| A `Spec` for the GraphQL `Float` type that decodes to an Elm `Float`.
-}
float : Spec NonNull FloatType variableSource Float
float =
    primitiveSpec FloatType Decode.float


{-| A `Spec` for the GraphQL `String` type that decodes to an Elm `String`.
-}
string : Spec NonNull StringType variableSource String
string =
    primitiveSpec StringType Decode.string


{-| A `Spec` for the GraphQL `Boolean` type that decodes to an Elm `Bool`.
-}
bool : Spec NonNull BooleanType variableSource Bool
bool =
    primitiveSpec BooleanType Decode.bool


{-| A `Spec` for the GraphQL `ID` type that decodes to an Elm `String`.
-}
id : Spec NonNull IdType variableSource String
id =
    primitiveSpec IdType Decode.string


{-| Constructs a `Spec` for a GraphQL Enum type. Takes a list of string-result pairs to map Enum values to `result` values. For example:

    type AccessLevel
        = AdminAccess
        | MemberAccess


    userAccessLevelField : Spec NonNull EnumType variableSource AccessLevel
    userAccessLevelField =
        (field "accessLevel"
            []
            (enum
                [ ( "ADMIN", AdminAccess )
                , ( "MEMBER", MemberAccess )
                ]
            )
        )
-}
enum : List ( String, result ) -> Spec NonNull EnumType variableSource result
enum =
    enumWithFallback
        (\label ->
            Decode.fail ("Unexpected enum value " ++ toString label)
        )


{-| Constructs a `Spec` for a GraphQL Enum type. Works the same as `enum`, but takes a default function to produce a `result` value from any Enum value not specified in the list of known Enum values. This is useful if you expect a schema to add more possible values to an Enum type in the future and don't want to bail out on the decoding process every time you encounter something you haven't seen before:

    type AccessLevel
        = AdminAccess
        | MemberAccess
        | UnknownAccess String


    userAccessLevelField : Spec NonNull EnumType variableSource AccessLevel
    userAccessLevelField =
        (field "accessLevel"
            []
            (enumWithDefault UnknownAccess
                [ ( "ADMIN", AdminAccess )
                , ( "MEMBER", MemberAccess )
                ]
            )
        )
-}
enumWithDefault : (String -> result) -> List ( String, result ) -> Spec NonNull EnumType variableSource result
enumWithDefault ctr =
    enumWithFallback
        (\label ->
            Decode.succeed (ctr label)
        )


enumWithFallback : (String -> Decoder a) -> List ( String, a ) -> Spec NonNull EnumType variableSource a
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
        Spec
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


decoderFromEnumLabel : (String -> Decoder a) -> List ( String, a ) -> String -> Decoder a
decoderFromEnumLabel fallbackDecoder labelledValues =
    let
        valueFromLabel =
            flip Dict.get (Dict.fromList labelledValues)

        decoder enumString =
            case valueFromLabel enumString of
                Just value ->
                    Decode.succeed value

                Nothing ->
                    fallbackDecoder enumString
    in
        decoder


{-| Constructs a `Spec` for a GraphQL List type. Takes any kind of `Spec` to use for the items of the list, and returns a `Spec` that decodes into an Elm `List`.
-}
list :
    Spec itemNullability itemType variableSource result
    -> Spec NonNull (ListType itemNullability itemType) variableSource (List result)
list (Spec itemType decoder vars fragments) =
    Spec
        (SpecifiedType
            { nullability = nonNullFlag
            , coreType = (ListType itemType)
            , join = listJoin
            , selectionSet = selectionSetFromSourceType itemType
            }
        )
        (Decode.list << decoder)
        vars
        fragments


{-| Transforms a `NonNull` `Spec` into one that allows `null` values, using a `Maybe` of the original `Spec`'s `result` type to represent the nullability in the decoded Elm value.

Note that the default `nullability` of a `Spec` in this module is `NonNull`. This is the opposite of the situation in the GraphQL schema language, whose types must be annotated with the Non-Null (`!`) modifier in order to specify that their values will never be `null`.
-}
nullable : Spec NonNull coreType variableSource result -> Spec Nullable coreType variableSource (Maybe result)
nullable (Spec sourceType decoder vars fragments) =
    case sourceType of
        SpecifiedType typeInfo ->
            Spec
                (SpecifiedType { typeInfo | nullability = nullableFlag })
                (Decode.nullable << decoder)
                vars
                fragments

        AnyType ->
            Spec AnyType (Decode.nullable << decoder) vars fragments


emptyObjectSpecifiedType : SourceType NonNull ObjectType
emptyObjectSpecifiedType =
    SpecifiedType
        { nullability = nonNullFlag
        , coreType = ObjectType
        , join = always
        , selectionSet = emptySelectionSet
        }


{-| Construct a `Spec` that always decodes to the given `result`, without using anything from the response value.
-}
produce : result -> Spec nullability coreType variableSource result
produce x =
    Spec AnyType (always (Decode.succeed x)) [] []


{-| Transform the result of a `Spec`'s decoder using the given function.

    type alias User =
        { nameLength : Int
        , email : String
        }

    object User
        |> withField "name" [] (map String.length string)
        |> withFeild "email" [] string
-}
map : (a -> b) -> Spec nullability coreType variableSource a -> Spec nullability coreType variableSource b
map f (Spec sourceType decoder vars fragments) =
    Spec sourceType (decoder >> Decode.map f) vars fragments


{-| Combine two different `Spec`s using a function that combines their decoding results. For example, you can use this function to construct a `Spec` for an object with two fields, like so:

    type alias User =
        { name : String
        , adminAccess : Bool
        }

    userSpec : Spec NonNull ObjectType variableSource User
    userSpec =
        map2 User
            (field "name" [] string)
            (field "adminAccess" [] bool)

The above is equivalent to the "pipeline style" approach using the `object` and `withField` functions. However, the `mapN` functions can give you better error messages from the compiler when your types don't quite line up properly.

You can also use the `mapN` functions to combine multiple fields in a response object into a single field in your decoded Elm type:

    type alias User =
        { name : String
        , adminAccess : Bool
        }

    joinName : String -> String -> String
    joinName first last =
        first ++ " " ++ last

    userSpec2 : Spec NonNull ObjectType variableSource User
    userSpec2 =
        map2 User
            (map2 joinName
                (field "firstName" [] string)
                (field "lastName" [] string)
            )
            (field "adminAccess" [] bool)

The above `userSpec2` ends up producing a GraphQL selection set of `{ firstName lastName adminAccess }`, and its decoder produces the `User`'s `name` by joining together the `firstName` and `lastName` values from the response with a single space.

You might find other situations where the `mapN` functions are useful, but combining object fields is by far the most common use case.
-}
map2 :
    (a -> b -> c)
    -> Spec nullability coreType variableSource a
    -> Spec nullability coreType variableSource b
    -> Spec nullability coreType variableSource c
map2 f (Spec sourceTypeA decoderA varsA fragmentsA) (Spec sourceTypeB decoderB varsB fragmentsB) =
    let
        joinedSourceType =
            join sourceTypeA sourceTypeB

        joinedDecoder selectionSet =
            Decode.map2 f (decoderA selectionSet) (decoderB selectionSet)

        mergedVariables =
            mergeVariables varsA varsB

        mergedFragments =
            mergeFragments fragmentsA fragmentsB
    in
        Spec joinedSourceType joinedDecoder mergedVariables mergedFragments


{-| Like `map2`, but combines three `Spec` values with a three-argument function.
-}
map3 :
    (a -> b -> c -> d)
    -> Spec nullability coreType variableSource a
    -> Spec nullability coreType variableSource b
    -> Spec nullability coreType variableSource c
    -> Spec nullability coreType variableSource d
map3 f s1 s2 s3 =
    map f s1
        |> with s2
        |> with s3


{-| Like `map2`, but combines four `Spec` values with a four-argument function.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> Spec nullability coreType variableSource a
    -> Spec nullability coreType variableSource b
    -> Spec nullability coreType variableSource c
    -> Spec nullability coreType variableSource d
    -> Spec nullability coreType variableSource e
map4 f s1 s2 s3 s4 =
    map f s1
        |> with s2
        |> with s3
        |> with s4


{-| Like `map2`, but combines five `Spec` values with a five-argument function.
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Spec nullability coreType variableSource a
    -> Spec nullability coreType variableSource b
    -> Spec nullability coreType variableSource c
    -> Spec nullability coreType variableSource d
    -> Spec nullability coreType variableSource e
    -> Spec nullability coreType variableSource f
map5 f s1 s2 s3 s4 s5 =
    map f s1
        |> with s2
        |> with s3
        |> with s4
        |> with s5


{-| Like `map2`, but combines six `Spec` values with a six-argument function.
-}
map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Spec nullability coreType variableSource a
    -> Spec nullability coreType variableSource b
    -> Spec nullability coreType variableSource c
    -> Spec nullability coreType variableSource d
    -> Spec nullability coreType variableSource e
    -> Spec nullability coreType variableSource f
    -> Spec nullability coreType variableSource g
map6 f s1 s2 s3 s4 s5 s6 =
    map f s1
        |> with s2
        |> with s3
        |> with s4
        |> with s5
        |> with s6


{-| Like `map2`, but combines seven `Spec` values with a seven-argument function.
-}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> Spec nullability coreType variableSource a
    -> Spec nullability coreType variableSource b
    -> Spec nullability coreType variableSource c
    -> Spec nullability coreType variableSource d
    -> Spec nullability coreType variableSource e
    -> Spec nullability coreType variableSource f
    -> Spec nullability coreType variableSource g
    -> Spec nullability coreType variableSource h
map7 f s1 s2 s3 s4 s5 s6 s7 =
    map f s1
        |> with s2
        |> with s3
        |> with s4
        |> with s5
        |> with s6
        |> with s7


{-| Like `map2`, but combines eight `Spec` values with an eight-argument function.
-}
map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i)
    -> Spec nullability coreType variableSource a
    -> Spec nullability coreType variableSource b
    -> Spec nullability coreType variableSource c
    -> Spec nullability coreType variableSource d
    -> Spec nullability coreType variableSource e
    -> Spec nullability coreType variableSource f
    -> Spec nullability coreType variableSource g
    -> Spec nullability coreType variableSource h
    -> Spec nullability coreType variableSource i
map8 f s1 s2 s3 s4 s5 s6 s7 s8 =
    map f s1
        |> with s2
        |> with s3
        |> with s4
        |> with s5
        |> with s6
        |> with s7
        |> with s8


{-| This is the general-purpose alternative to the `mapN` functions, usable in a pipeline to combine any number of `Spec` values:

    type alias User =
        { name : String
        , adminAccess : Bool
        }

    joinName : String -> String -> String
    joinName first last =
        first ++ " " ++ last

    userSpec : Spec NonNull ObjectType variableSource User
    userSpec =
        object User
            |> with
                (map2 joinName
                    (field "firstName" [] string)
                    (field "lastName" [] string)
                )
            |> withField "adminAccess" [] bool

This function forms the basis of `withField`, `withFragment`, and `withInlineFragment`, which are provided for convenience. In the above code, `withField "adminAccess" [] bool` is equivalent to `with (field "adminAccess" [] bool)`.
-}
with : Spec nullability coreType variableSource a -> Spec nullability coreType variableSource (a -> b) -> Spec nullability coreType variableSource b
with specA specF =
    map2 (<|) specF specA


applyFieldOption : FieldOption variableSource -> AST.FieldInfo -> AST.FieldInfo
applyFieldOption fieldOption field =
    case fieldOption of
        FieldAlias name ->
            { field | alias = Just name }

        FieldArgs arguments ->
            { field
                | arguments =
                    field.arguments
                        ++ List.map (Tuple.mapSecond Arg.getAST) arguments
            }

        FieldDirective name arguments ->
            { field
                | directives =
                    field.directives
                        ++ [ AST.Directive
                                { name = name
                                , arguments = List.map (Tuple.mapSecond Arg.getAST) arguments
                                }
                           ]
            }


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
                    | coreType = (typeInfoA.join typeInfoA.coreType typeInfoB.coreType)
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


selectionSetFromSpec : Spec nullability coreType variableSource result -> AST.SelectionSet
selectionSetFromSpec (Spec sourceType _ _ _) =
    selectionSetFromSourceType sourceType


emptySelectionSet : AST.SelectionSet
emptySelectionSet =
    AST.SelectionSet []


primitiveSpec : coreType -> Decoder result -> Spec NonNull coreType variableSource result
primitiveSpec coreType decoder =
    Spec
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
    Spec nullability coreType variableSource result
    -> List AST.VariableDefinition
variableDefinitionsAST (Spec _ _ vars _) =
    List.map Variable.toDefinitionAST vars


directiveAST :
    ( String, List ( String, Arg.Value a ) )
    -> AST.Directive
directiveAST ( name, arguments ) =
    AST.Directive
        { name = name
        , arguments = List.map (Tuple.mapSecond Arg.getAST) arguments
        }


operationTypeAST : OperationType operationType -> AST.OperationType
operationTypeAST operationType =
    case operationType of
        QueryOperationType ->
            AST.Query

        MutationOperationType ->
            AST.Mutation


operationAST : Operation operationType variableSource result -> AST.OperationDefinitionInfo
operationAST (Operation { operationType, name, directives, spec }) =
    { operationType = operationTypeAST operationType
    , name = name
    , variableDefinitions = variableDefinitionsAST spec
    , directives = List.map directiveAST directives
    , selectionSet = selectionSetFromSpec spec
    }


fragmentAST : Fragment variableSource result -> AST.FragmentDefinitionInfo
fragmentAST (Fragment { name, typeCondition, directives, spec }) =
    { name = name
    , typeCondition = typeCondition
    , directives = List.map directiveAST directives
    , selectionSet = selectionSetFromSpec spec
    }


varsFromDirectives : List ( String, List ( String, Arg.Value variableSource ) ) -> List (Variable variableSource)
varsFromDirectives =
    List.concatMap (Tuple.second >> varsFromArguments)


fragmentVariables : Fragment variableSource result -> List (Variable variableSource)
fragmentVariables (Fragment { directives, spec }) =
    let
        directiveVariables =
            varsFromDirectives directives

        (Spec _ _ specVariables _) =
            spec
    in
        mergeVariables directiveVariables specVariables


documentAST : Document operationType variableSource result -> AST.Document
documentAST (Document { ast }) =
    ast


documentString : Document operationType variableSource result -> String
documentString (Document { serialized }) =
    serialized


documentResponseDecoder :
    Document operationType variableSource result
    -> Decoder result
documentResponseDecoder (Document { operation }) =
    let
        (Operation { spec }) =
            operation
    in
        specDecoder spec


documentVariables : Document operationType variableSource result -> List (Variable variableSource)
documentVariables (Document { operation }) =
    let
        (Operation { spec }) =
            operation

        (Spec _ _ vars _) =
            spec
    in
        vars


specDecoder : Spec nullability coreType variableSource result -> Decoder result
specDecoder (Spec sourceType decoderFromSelectionSet _ _) =
    sourceType
        |> selectionSetFromSourceType
        |> decoderFromSelectionSet


equalVariableDefinitionAST : Variable a -> Variable a -> Bool
equalVariableDefinitionAST varA varB =
    Variable.toDefinitionAST varA == Variable.toDefinitionAST varB


mergeVariables : List (Variable source) -> List (Variable source) -> List (Variable source)
mergeVariables varsA varsB =
    varsA
        ++ (varsB
                |> List.filter
                    (\var -> not (List.any (equalVariableDefinitionAST var) varsA))
           )


mergeFragments : List AST.FragmentDefinitionInfo -> List AST.FragmentDefinitionInfo -> List AST.FragmentDefinitionInfo
mergeFragments fragmentsA fragmentsB =
    fragmentsA
        ++ (fragmentsB
                |> List.filter
                    (\fragment -> not (List.any ((==) fragment) fragmentsA))
           )
