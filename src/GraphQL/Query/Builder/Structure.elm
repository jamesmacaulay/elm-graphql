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
        , OpType(..)
        , Op
        , OpOption
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
        , opName
        , opVariable
        , opVariableWithDefault
        , opDirective
        , op
        , query
        , mutation
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


type OpType
    = Query
    | Mutation


type alias Op =
    { opType : OpType
    , name : Maybe String
    , variables : List VariableDefinition
    , directives : List Directive
    , spec : Spec
    }


type OpOption
    = OpName String
    | OpVariable String String (Maybe Arg.Value)
    | OpDirective String (List ( String, Arg.Value ))


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


opName : String -> OpOption
opName =
    OpName


opVariable : String -> String -> OpOption
opVariable name variableType =
    OpVariable name variableType Nothing


opVariableWithDefault : String -> String -> Arg.Value -> OpOption
opVariableWithDefault name variableType defaultValue =
    OpVariable name variableType (Just defaultValue)


opDirective : String -> List ( String, Arg.Value ) -> OpOption
opDirective =
    OpDirective


applyOpOption : OpOption -> Op -> Op
applyOpOption queryOption query =
    case queryOption of
        OpName name ->
            { query | name = Just name }

        OpVariable name variableType defaultValue ->
            { query
                | variables =
                    query.variables
                        ++ [ { name = name
                             , variableType = variableType
                             , defaultValue = defaultValue
                             }
                           ]
            }

        OpDirective name args ->
            { query
                | directives =
                    query.directives
                        ++ [ { name = name
                             , args = args
                             }
                           ]
            }


op : OpType -> List OpOption -> Spec -> Op
op opType opOptions spec =
    let
        objectSpec =
            case spec of
                ObjectSpec _ ->
                    spec

                _ ->
                    ObjectSpec []

        init =
            { opType = opType
            , name = Nothing
            , variables = []
            , directives = []
            , spec = objectSpec
            }
    in
        List.foldl applyOpOption init opOptions


query : List OpOption -> Spec -> Op
query =
    op Query


mutation : List OpOption -> Spec -> Op
mutation =
    op Mutation
