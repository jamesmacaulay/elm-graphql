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
        , BuilderError(..)
        , Builder(..)
        , responseKey
        , getBaseSpec
        , mapBuilder
        , specBuilderIntersection
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
    , spec : Spec
    , directives : List Directive
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


type BuilderError
    = InvalidIntersection Spec Spec
    | InvalidFragment Spec


type Builder a
    = Builder (List BuilderError) a


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


mapBuilder : (a -> b) -> Builder a -> Builder b
mapBuilder f (Builder errs spec) =
    Builder errs (f spec)


appendSelections : List Selection -> List Selection -> List Selection
appendSelections a b =
    a ++ b


specIntersection : Spec -> Spec -> Result BuilderError Spec
specIntersection a b =
    case ( a, b ) of
        ( AnySpec, _ ) ->
            Ok b

        ( _, AnySpec ) ->
            Ok a

        ( ObjectSpec ssa, ObjectSpec ssb ) ->
            Ok (ObjectSpec (appendSelections ssa ssb))

        ( ListSpec innerA, ListSpec innerB ) ->
            specIntersection innerA innerB
                |> Result.map ListSpec

        ( NullableSpec innerA, NullableSpec innerB ) ->
            specIntersection innerA innerB
                |> Result.map NullableSpec

        ( IntSpec, IntSpec ) ->
            Ok IntSpec

        ( FloatSpec, FloatSpec ) ->
            Ok FloatSpec

        ( StringSpec, StringSpec ) ->
            Ok StringSpec

        ( BooleanSpec, BooleanSpec ) ->
            Ok BooleanSpec

        _ ->
            Err (InvalidIntersection a b)


specBuilderIntersection : Builder Spec -> Builder Spec -> Builder Spec
specBuilderIntersection (Builder errsA specA) (Builder errsB specB) =
    case specIntersection specA specB of
        Ok spec ->
            Builder (errsA ++ errsB) spec

        Err builderError ->
            Builder (errsA ++ errsB ++ [ builderError ]) specA


string : Builder Spec
string =
    Builder [] StringSpec


int : Builder Spec
int =
    Builder [] IntSpec


float : Builder Spec
float =
    Builder [] FloatSpec


bool : Builder Spec
bool =
    Builder [] BooleanSpec


list : Builder Spec -> Builder Spec
list =
    mapBuilder ListSpec


nullable : Builder Spec -> Builder Spec
nullable =
    mapBuilder NullableSpec


any : Builder Spec
any =
    Builder [] AnySpec


object : Builder Spec
object =
    Builder [] (ObjectSpec [])


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
