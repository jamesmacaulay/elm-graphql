module GraphQL.Query.Builder.Structure
    exposing
        ( Selection(..)
        , SpecD(..)
        , Spec(..)
        , IntSpec(..)
        , StringSpec(..)
        , FloatSpec(..)
        , BooleanSpec(..)
        , ListSpec(..)
        , NullableSpec(..)
        , ObjectSpec(..)
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


type SpecD
    = AnySpecD
    | IntSpecD
    | FloatSpecD
    | StringSpecD
    | BooleanSpecD
    | ObjectSpecD SelectionSet
    | ListSpecD SpecD
    | NullableSpecD SpecD


type IntSpec
    = IntSpec


type FloatSpec
    = FloatSpec


type StringSpec
    = StringSpec


type BooleanSpec
    = BooleanSpec


type ListSpec a
    = ListSpec (Spec a)


type NullableSpec a
    = NullableSpec (Spec a)


type ObjectSpec
    = ObjectSpec SelectionSet


type Spec a
    = Spec a (Spec a -> Spec a -> Spec a) SpecD
    | AnySpec


type alias Field =
    { name : String
    , spec : SpecD
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
    , spec : Spec ObjectSpec
    }


type alias FragmentSpread =
    { name : String
    , directives : List Directive
    }


type alias InlineFragment =
    { typeCondition : Maybe String
    , directives : List Directive
    , spec : Spec ObjectSpec
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
    , spec : Spec ObjectSpec
    }


type OperationOption
    = OperationName String
    | OperationVariable String String (Maybe Arg.Value)
    | OperationDirective String (List ( String, Arg.Value ))


type QueryOption
    = QueryOption OperationOption


type MutationOption
    = MutationOption OperationOption


getSpecFromStructure : Spec a -> SpecD
getSpecFromStructure structure =
    case structure of
        Spec _ _ spec ->
            spec

        AnySpec ->
            AnySpecD


responseKey : Field -> String
responseKey field =
    Maybe.withDefault field.name field.fieldAlias


getBaseSpec : SpecD -> SpecD
getBaseSpec spec =
    case spec of
        ListSpecD inner ->
            getBaseSpec inner

        NullableSpecD inner ->
            getBaseSpec inner

        _ ->
            spec


appendSelections : List Selection -> List Selection -> List Selection
appendSelections a b =
    a ++ b


objectJoin : Spec ObjectSpec -> Spec ObjectSpec -> Spec ObjectSpec
objectJoin a b =
    case ( a, b ) of
        ( _, AnySpec ) ->
            a

        ( AnySpec, _ ) ->
            b

        ( Spec (ObjectSpec selectionsA) _ _, Spec (ObjectSpec selectionsB) _ _ ) ->
            let
                selections =
                    appendSelections selectionsA selectionsB
            in
                Spec (ObjectSpec selections) objectJoin (ObjectSpecD selections)


listJoin : Spec (ListSpec a) -> Spec (ListSpec a) -> Spec (ListSpec a)
listJoin a b =
    case ( a, b ) of
        ( _, AnySpec ) ->
            a

        ( AnySpec, _ ) ->
            b

        ( Spec (ListSpec wrappedSpecA) _ _, Spec (ListSpec wrappedSpecB) _ _ ) ->
            let
                wrappedSpec =
                    join wrappedSpecA wrappedSpecB
            in
                Spec (ListSpec wrappedSpec) listJoin (ListSpecD (getSpecFromStructure wrappedSpec))


nullableJoin : Spec (NullableSpec a) -> Spec (NullableSpec a) -> Spec (NullableSpec a)
nullableJoin a b =
    case ( a, b ) of
        ( _, AnySpec ) ->
            a

        ( AnySpec, _ ) ->
            b

        ( Spec (NullableSpec wrappedSpecA) _ _, Spec (NullableSpec wrappedSpecB) _ _ ) ->
            let
                wrappedSpec =
                    join wrappedSpecA wrappedSpecB
            in
                Spec (NullableSpec wrappedSpec) nullableJoin (NullableSpecD (getSpecFromStructure wrappedSpec))


primitiveJoin : Spec a -> Spec a -> Spec a
primitiveJoin a b =
    case ( a, b ) of
        ( AnySpec, _ ) ->
            b

        _ ->
            a


join : Spec a -> Spec a -> Spec a
join a b =
    case ( a, b ) of
        ( AnySpec, _ ) ->
            b

        ( Spec _ joinFn _, _ ) ->
            joinFn a b


string : Spec StringSpec
string =
    Spec StringSpec primitiveJoin StringSpecD


int : Spec IntSpec
int =
    Spec IntSpec primitiveJoin IntSpecD


float : Spec FloatSpec
float =
    Spec FloatSpec primitiveJoin FloatSpecD


bool : Spec BooleanSpec
bool =
    Spec BooleanSpec primitiveJoin BooleanSpecD


list : Spec a -> Spec (ListSpec a)
list structure =
    Spec (ListSpec structure) listJoin (ListSpecD (getSpecFromStructure structure))


nullable : Spec a -> Spec (NullableSpec a)
nullable structure =
    Spec (NullableSpec structure) nullableJoin (NullableSpecD (getSpecFromStructure structure))


any : Spec a
any =
    AnySpec


object : Spec ObjectSpec
object =
    Spec (ObjectSpec []) objectJoin (ObjectSpecD [])


objectFromSelection : Selection -> Spec ObjectSpec
objectFromSelection selection =
    let
        selections =
            [ selection ]
    in
        Spec (ObjectSpec selections) objectJoin (ObjectSpecD selections)


objectFromField : Field -> Spec ObjectSpec
objectFromField field =
    objectFromSelection (FieldSelection field)


objectFromFragmentSpread : FragmentSpread -> Spec ObjectSpec
objectFromFragmentSpread fragmentSpread =
    objectFromSelection (FragmentSpreadSelection fragmentSpread)


objectFromInlineFragment : InlineFragment -> Spec ObjectSpec
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


field : String -> List FieldOption -> Spec a -> Field
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
    -> Spec ObjectSpec
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


query : List QueryOption -> Spec ObjectSpec -> Query
query =
    operation Query (\(QueryOption option) -> option)


mutation : List MutationOption -> Spec ObjectSpec -> Mutation
mutation =
    operation Mutation (\(MutationOption option) -> option)
