module GraphQL.Query.Builder.Structure exposing (..)

import GraphQL.Query.Arg as Arg


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
    | MaybeSpec Spec


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
    | FieldDirectives (List Directive)


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


type alias Query =
    { name : Maybe String
    , spec : Spec
    }


type QueryOption
    = QueryName String


getBaseSpec : Spec -> Spec
getBaseSpec spec =
    case spec of
        ListSpec inner ->
            getBaseSpec inner

        MaybeSpec inner ->
            getBaseSpec inner

        _ ->
            spec
