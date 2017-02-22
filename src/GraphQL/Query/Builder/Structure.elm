module GraphQL.Query.Builder.Structure
    exposing
        ( Selection(..)
        , Spec(..)
        , Structure(..)
        , IntTypedSpec(..)
        , StringTypedSpec(..)
        , FloatTypedSpec(..)
        , BooleanTypedSpec(..)
        , ListTypedSpec(..)
        , NullableTypedSpec(..)
        , ObjectTypedSpec(..)
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
        , getSpecFromStructure
        , join
        , string
        , int
        , float
        , bool
        , list
        , nullable
        , any
        , object
        , objectFromField
        , objectFromFragmentSpread
        , objectFromInlineFragment
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


type alias SelectionSet =
    List Selection


type Spec
    = AnySpec
    | IntSpec
    | FloatSpec
    | StringSpec
    | BooleanSpec
    | ObjectSpec SelectionSet
    | ListSpec Spec
    | NullableSpec Spec


type IntTypedSpec
    = IntTypedSpec


type FloatTypedSpec
    = FloatTypedSpec


type StringTypedSpec
    = StringTypedSpec


type BooleanTypedSpec
    = BooleanTypedSpec


type ListTypedSpec a
    = ListTypedSpec (Structure a)


type NullableTypedSpec a
    = NullableTypedSpec (Structure a)


type ObjectTypedSpec
    = ObjectTypedSpec SelectionSet


type Structure a
    = Structure a (Structure a -> Structure a -> Structure a) Spec
    | AnyStructure


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
    , spec : Structure ObjectTypedSpec
    }


type alias FragmentSpread =
    { name : String
    , directives : List Directive
    }


type alias InlineFragment =
    { typeCondition : Maybe String
    , directives : List Directive
    , spec : Structure ObjectTypedSpec
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
    , spec : Structure ObjectTypedSpec
    }


type OperationOption
    = OperationName String
    | OperationVariable String String (Maybe Arg.Value)
    | OperationDirective String (List ( String, Arg.Value ))


type QueryOption
    = QueryOption OperationOption


type MutationOption
    = MutationOption OperationOption


getSpecFromStructure : Structure a -> Spec
getSpecFromStructure structure =
    case structure of
        Structure _ _ spec ->
            spec

        AnyStructure ->
            AnySpec


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


objectJoin : Structure ObjectTypedSpec -> Structure ObjectTypedSpec -> Structure ObjectTypedSpec
objectJoin a b =
    case ( a, b ) of
        ( _, AnyStructure ) ->
            a

        ( AnyStructure, _ ) ->
            b

        ( Structure (ObjectTypedSpec selectionsA) _ _, Structure (ObjectTypedSpec selectionsB) _ _ ) ->
            let
                selections =
                    appendSelections selectionsA selectionsB
            in
                Structure (ObjectTypedSpec selections) objectJoin (ObjectSpec selections)


listJoin : Structure (ListTypedSpec a) -> Structure (ListTypedSpec a) -> Structure (ListTypedSpec a)
listJoin a b =
    case ( a, b ) of
        ( _, AnyStructure ) ->
            a

        ( AnyStructure, _ ) ->
            b

        ( Structure (ListTypedSpec wrappedStructureA) _ _, Structure (ListTypedSpec wrappedStructureB) _ _ ) ->
            let
                wrappedStructure =
                    join wrappedStructureA wrappedStructureB
            in
                Structure (ListTypedSpec wrappedStructure) listJoin (ListSpec (getSpecFromStructure wrappedStructure))


nullableJoin : Structure (NullableTypedSpec a) -> Structure (NullableTypedSpec a) -> Structure (NullableTypedSpec a)
nullableJoin a b =
    case ( a, b ) of
        ( _, AnyStructure ) ->
            a

        ( AnyStructure, _ ) ->
            b

        ( Structure (NullableTypedSpec wrappedStructureA) _ _, Structure (NullableTypedSpec wrappedStructureB) _ _ ) ->
            let
                wrappedStructure =
                    join wrappedStructureA wrappedStructureB
            in
                Structure (NullableTypedSpec wrappedStructure) nullableJoin (NullableSpec (getSpecFromStructure wrappedStructure))


primitiveJoin : Structure a -> Structure a -> Structure a
primitiveJoin a b =
    case ( a, b ) of
        ( AnyStructure, _ ) ->
            b

        _ ->
            a


join : Structure a -> Structure a -> Structure a
join a b =
    case ( a, b ) of
        ( AnyStructure, _ ) ->
            b

        ( Structure _ joinFn _, _ ) ->
            joinFn a b


string : Structure StringTypedSpec
string =
    Structure StringTypedSpec primitiveJoin StringSpec


int : Structure IntTypedSpec
int =
    Structure IntTypedSpec primitiveJoin IntSpec


float : Structure FloatTypedSpec
float =
    Structure FloatTypedSpec primitiveJoin FloatSpec


bool : Structure BooleanTypedSpec
bool =
    Structure BooleanTypedSpec primitiveJoin BooleanSpec


list : Structure a -> Structure (ListTypedSpec a)
list structure =
    Structure (ListTypedSpec structure) listJoin (ListSpec (getSpecFromStructure structure))


nullable : Structure a -> Structure (NullableTypedSpec a)
nullable structure =
    Structure (NullableTypedSpec structure) nullableJoin (NullableSpec (getSpecFromStructure structure))


any : Structure a
any =
    AnyStructure


object : Structure ObjectTypedSpec
object =
    Structure (ObjectTypedSpec []) objectJoin (ObjectSpec [])


objectFromSelection : Selection -> Structure ObjectTypedSpec
objectFromSelection selection =
    let
        selections =
            [ selection ]
    in
        Structure (ObjectTypedSpec selections) objectJoin (ObjectSpec selections)


objectFromField : Field -> Structure ObjectTypedSpec
objectFromField field =
    objectFromSelection (FieldSelection field)


objectFromFragmentSpread : FragmentSpread -> Structure ObjectTypedSpec
objectFromFragmentSpread fragmentSpread =
    objectFromSelection (FragmentSpreadSelection fragmentSpread)


objectFromInlineFragment : InlineFragment -> Structure ObjectTypedSpec
objectFromInlineFragment inlineFragment =
    objectFromSelection (InlineFragmentSelection inlineFragment)


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


field : String -> List FieldOption -> Structure a -> Field
field name fieldOptions structure =
    let
        init =
            { name = name
            , spec = getSpecFromStructure structure
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
    -> Structure ObjectTypedSpec
    -> operationType
operation operationConstructor optionDeconstructor options spec =
    let
        init =
            { name = Nothing
            , variables = []
            , directives = []
            , spec = spec
            }
    in
        options
            |> List.foldl (optionDeconstructor >> applyOperationOption) init
            |> operationConstructor


query : List QueryOption -> Structure ObjectTypedSpec -> Query
query =
    operation Query (\(QueryOption option) -> option)


mutation : List MutationOption -> Structure ObjectTypedSpec -> Mutation
mutation =
    operation Mutation (\(MutationOption option) -> option)
