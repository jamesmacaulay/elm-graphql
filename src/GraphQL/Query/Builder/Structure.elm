module GraphQL.Query.Builder.Structure
    exposing
        ( Selection(..)
        , Spec(..)
        , Field
        , FieldOption
        , Directive
        , FragmentDefinition
        , FragmentSpread
        , InlineFragment
        , VariableDefinition
        , Query(..)
        , QueryOption
        , Mutation(..)
        , MutationOption
        , OperationConfig
        , responseKey
        , getBaseSpec
        , join
        , string
        , int
        , float
        , bool
        , list
        , nullable
        , any
        , object
        , fieldAlias
        , fieldArgs
        , fieldDirective
        , field
        , query
        , queryName
        , queryVariable
        , queryVariableWithDefault
        , queryDirective
        , mutation
        , mutationName
        , mutationVariable
        , mutationVariableWithDefault
        , mutationDirective
        )

import GraphQL.Query.Builder.Arg as Arg


type Selection
    = FieldSelection Field
    | FragmentSpreadSelection FragmentSpread
    | InlineFragmentSelection InlineFragment


type Spec
    = AnySpec
    | IntSpec
    | FloatSpec
    | StringSpec
    | BooleanSpec
    | ObjectSpec (List Selection)
    | ListSpec Spec
    | NullableSpec Spec


type alias Field =
    { name : String
    , spec : Spec
    , fieldAlias : Maybe String
    , args : List ( String, Arg.Value )
    , directives : List Directive
    }


type FieldOption
    = FieldAlias String
    | FieldArgs (List ( String, Arg.Value ))
    | FieldDirective String (List ( String, Arg.Value ))


type alias Directive =
    { name : String
    , args : List ( String, Arg.Value )
    }


type alias FragmentDefinition =
    { name : String
    , typeCondition : String
    , directives : List Directive
    , spec : Spec
    }


type alias FragmentSpread =
    { name : String
    , directives : List Directive
    }


type alias InlineFragment =
    { typeCondition : Maybe String
    , directives : List Directive
    , spec : Spec
    }


type alias VariableDefinition =
    { name : String
    , variableType : String
    , defaultValue : Maybe Arg.Value
    }


type Query
    = Query OperationConfig


type Mutation
    = Mutation OperationConfig


type alias OperationConfig =
    { name : Maybe String
    , variables : List VariableDefinition
    , directives : List Directive
    , spec : Spec
    }


type OperationOption
    = OperationName String
    | OperationVariable String String (Maybe Arg.Value)
    | OperationDirective String (List ( String, Arg.Value ))


type QueryOption
    = QueryOption OperationOption


type MutationOption
    = MutationOption OperationOption


responseKey : Field -> String
responseKey field =
    Maybe.withDefault field.name field.fieldAlias


getBaseSpec : Spec -> Spec
getBaseSpec spec =
    case spec of
        ListSpec inner ->
            getBaseSpec inner

        NullableSpec inner ->
            getBaseSpec inner

        _ ->
            spec


appendSelections : List Selection -> List Selection -> List Selection
appendSelections a b =
    a ++ b


join : Spec -> Spec -> Spec
join a b =
    case ( a, b ) of
        ( AnySpec, _ ) ->
            b

        ( ObjectSpec selectionsA, ObjectSpec selectionsB ) ->
            ObjectSpec (appendSelections selectionsA selectionsB)

        ( ListSpec wrappedA, ListSpec wrappedB ) ->
            ListSpec (join wrappedA wrappedB)

        ( NullableSpec wrappedA, NullableSpec wrappedB ) ->
            NullableSpec (join wrappedA wrappedB)

        _ ->
            a


string : Spec
string =
    StringSpec


int : Spec
int =
    IntSpec


float : Spec
float =
    FloatSpec


bool : Spec
bool =
    BooleanSpec


list : Spec -> Spec
list =
    ListSpec


nullable : Spec -> Spec
nullable =
    NullableSpec


any : Spec
any =
    AnySpec


object : Spec
object =
    ObjectSpec []


fieldAlias : String -> FieldOption
fieldAlias =
    FieldAlias


fieldArgs : List ( String, Arg.Value ) -> FieldOption
fieldArgs =
    FieldArgs


fieldDirective : String -> List ( String, Arg.Value ) -> FieldOption
fieldDirective =
    FieldDirective


applyFieldOption : FieldOption -> Field -> Field
applyFieldOption fieldOption field =
    case fieldOption of
        FieldAlias name ->
            { field | fieldAlias = Just name }

        FieldArgs args ->
            { field | args = field.args ++ args }

        FieldDirective name args ->
            { field
                | directives =
                    field.directives
                        ++ [ { name = name
                             , args = args
                             }
                           ]
            }


field : String -> List FieldOption -> Spec -> Field
field name fieldOptions spec =
    let
        init =
            { name = name
            , spec = spec
            , fieldAlias = Nothing
            , args = []
            , directives = []
            }
    in
        List.foldl applyFieldOption init fieldOptions


queryName : String -> QueryOption
queryName =
    OperationName >> QueryOption


mutationName : String -> MutationOption
mutationName =
    OperationName >> MutationOption


queryVariable : String -> String -> QueryOption
queryVariable name variableType =
    QueryOption (OperationVariable name variableType Nothing)


mutationVariable : String -> String -> MutationOption
mutationVariable name variableType =
    MutationOption (OperationVariable name variableType Nothing)


queryVariableWithDefault : String -> String -> Arg.Value -> QueryOption
queryVariableWithDefault name variableType defaultValue =
    QueryOption (OperationVariable name variableType (Just defaultValue))


mutationVariableWithDefault : String -> String -> Arg.Value -> MutationOption
mutationVariableWithDefault name variableType defaultValue =
    MutationOption (OperationVariable name variableType (Just defaultValue))


queryDirective : String -> List ( String, Arg.Value ) -> QueryOption
queryDirective name args =
    QueryOption (OperationDirective name args)


mutationDirective : String -> List ( String, Arg.Value ) -> MutationOption
mutationDirective name args =
    MutationOption (OperationDirective name args)


applyOperationOption : OperationOption -> OperationConfig -> OperationConfig
applyOperationOption option config =
    case option of
        OperationName name ->
            { config | name = Just name }

        OperationVariable name variableType defaultValue ->
            { config
                | variables =
                    config.variables
                        ++ [ { name = name
                             , variableType = variableType
                             , defaultValue = defaultValue
                             }
                           ]
            }

        OperationDirective name args ->
            { config
                | directives =
                    config.directives
                        ++ [ { name = name
                             , args = args
                             }
                           ]
            }


operation :
    (OperationConfig -> operationType)
    -> (optionType -> OperationOption)
    -> List optionType
    -> Spec
    -> operationType
operation operationConstructor optionDeconstructor options spec =
    let
        objectSpec =
            case spec of
                ObjectSpec _ ->
                    spec

                _ ->
                    ObjectSpec []

        init =
            { name = Nothing
            , variables = []
            , directives = []
            , spec = objectSpec
            }
    in
        options
            |> List.foldl (optionDeconstructor >> applyOperationOption) init
            |> operationConstructor


query : List QueryOption -> Spec -> Query
query =
    operation Query (\(QueryOption option) -> option)


mutation : List MutationOption -> Spec -> Mutation
mutation =
    operation Mutation (\(MutationOption option) -> option)
