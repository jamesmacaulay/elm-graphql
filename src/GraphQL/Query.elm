module GraphQL.Query exposing (..)

import GraphQL.Query.Arg as Arg
import Json.Decode as Decode exposing (Decoder, (:=))


type Selection
    = FieldSelection Field
    | FragmentSpreadSelection FragmentSpread
    | InlineFragmentSelection InlineFragment


type BuilderError
    = InvalidIntersection Spec Spec
    | InvalidFragment Spec


type Spec
    = AnySpec
    | IntSpec
    | FloatSpec
    | StringSpec
    | BooleanSpec
    | ObjectSpec (List Selection)
    | ListSpec Spec
    | MaybeSpec Spec


getBaseSpec : Spec -> Spec
getBaseSpec spec =
    case spec of
        ListSpec inner ->
            getBaseSpec inner

        MaybeSpec inner ->
            getBaseSpec inner

        _ ->
            spec


type Builder a
    = Builder (List BuilderError) a


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

        ( MaybeSpec innerA, MaybeSpec innerB ) ->
            specIntersection innerA innerB
                |> Result.map MaybeSpec

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


type Field
    = Field
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


type Directive
    = Directive
        { name : String
        , args : List ( String, Arg.Value )
        }


type FragmentDefinition
    = FragmentDefinition
        { name : String
        , typeCondition : String
        , spec : Spec
        , directives : List Directive
        }


type FragmentSpread
    = FragmentSpread
        { name : String
        , directives : List Directive
        }


type InlineFragment
    = InlineFragment
        { typeCondition : Maybe String
        , directives : List Directive
        , spec : Spec
        }


type Query
    = Query
        { name : Maybe String
        , spec : Spec
        }


type QueryOption
    = QueryName String


type Decodable node result
    = Decodable node (Decoder result)


mapDecodable : (a -> b) -> (Decoder c -> Decoder d) -> Decodable a c -> Decodable b d
mapDecodable f g (Decodable node decoder) =
    Decodable (f node) (g decoder)


mapNode : (a -> b) -> Decodable a result -> Decodable b result
mapNode f =
    mapDecodable f identity


mapDecoder : (Decoder a -> Decoder b) -> Decodable node a -> Decodable node b
mapDecoder =
    mapDecodable identity


getNode : Decodable node result -> node
getNode (Decodable node _) =
    node


getDecoder : Decodable a result -> Decoder result
getDecoder (Decodable _ decoder) =
    decoder


string : Decodable (Builder Spec) String
string =
    Decodable (Builder [] StringSpec) Decode.string


int : Decodable (Builder Spec) Int
int =
    Decodable (Builder [] IntSpec) Decode.int


float : Decodable (Builder Spec) Float
float =
    Decodable (Builder [] FloatSpec) Decode.float


bool : Decodable (Builder Spec) Bool
bool =
    Decodable (Builder [] BooleanSpec) Decode.bool


list : Decodable (Builder Spec) a -> Decodable (Builder Spec) (List a)
list =
    mapDecodable (mapBuilder ListSpec) Decode.list


maybe : Decodable (Builder Spec) a -> Decodable (Builder Spec) (Maybe a)
maybe =
    let
        nullable decoder =
            Decode.oneOf [ Decode.map Just decoder, Decode.null Nothing ]
    in
        mapDecodable (mapBuilder MaybeSpec) nullable


construct : (a -> b) -> Decodable (Builder Spec) (a -> b)
construct constructor =
    Decodable (Builder [] AnySpec) (Decode.succeed constructor)


object : (a -> b) -> Decodable (Builder Spec) (a -> b)
object constructor =
    Decodable (Builder [] (ObjectSpec [])) (Decode.succeed constructor)


fieldAlias : String -> FieldOption
fieldAlias =
    FieldAlias


fieldArgs : List ( String, Arg.Value ) -> FieldOption
fieldArgs =
    FieldArgs


applyFieldOption : FieldOption -> Field -> Field
applyFieldOption fieldOption (Field fieldInfo) =
    case fieldOption of
        FieldAlias name ->
            Field { fieldInfo | fieldAlias = Just name }

        FieldArgs args ->
            Field { fieldInfo | args = fieldInfo.args ++ args }

        FieldDirectives directives ->
            Field { fieldInfo | directives = fieldInfo.directives ++ directives }


queryName : String -> QueryOption
queryName =
    QueryName


applyQueryOption : QueryOption -> Query -> Query
applyQueryOption queryOption (Query queryInfo) =
    case queryOption of
        QueryName name ->
            Query { queryInfo | name = Just name }


map : (a -> b) -> Decodable (Builder Spec) a -> Decodable (Builder Spec) b
map f =
    mapDecoder (Decode.map f)


andMap : Decodable (Builder Spec) a -> Decodable (Builder Spec) (a -> b) -> Decodable (Builder Spec) b
andMap (Decodable littleSpecBuilder littleDecoder) (Decodable bigSpecBuilder bigDecoder) =
    let
        specBuilder =
            specBuilderIntersection bigSpecBuilder littleSpecBuilder

        decoder =
            Decode.object2 (<|) bigDecoder littleDecoder
    in
        Decodable specBuilder decoder


field : String -> List FieldOption -> Decodable (Builder Spec) a -> Decodable (Builder Spec) a
field name fieldOptions (Decodable (Builder valueErrs valueSpec) valueDecoder) =
    let
        field =
            Field
                { name = name
                , spec = valueSpec
                , fieldAlias = Nothing
                , args = []
                , directives = []
                }
                |> flip (List.foldr applyFieldOption) fieldOptions

        spec =
            (ObjectSpec [ FieldSelection field ])

        decoder =
            (name := valueDecoder)
    in
        Decodable (Builder valueErrs spec) decoder


withField :
    String
    -> List FieldOption
    -> Decodable (Builder Spec) a
    -> Decodable (Builder Spec) (a -> b)
    -> Decodable (Builder Spec) b
withField name fieldOptions decodableFieldSpec decodableParentSpec =
    decodableParentSpec
        |> andMap (field name fieldOptions decodableFieldSpec)


extractField : String -> List FieldOption -> Decodable (Builder Spec) a -> Decodable (Builder Spec) a
extractField =
    field


fragmentSpread : Decodable (Builder FragmentDefinition) a -> List Directive -> Decodable (Builder Spec) (Maybe a)
fragmentSpread (Decodable (Builder fragmentErrs (FragmentDefinition { name })) fragmentDecoder) directives =
    let
        fragmentSpread =
            FragmentSpread
                { name = name
                , directives = directives
                }

        spec =
            ObjectSpec [ FragmentSpreadSelection fragmentSpread ]

        decoder =
            Decode.maybe fragmentDecoder
    in
        Decodable (Builder fragmentErrs spec) decoder


withFragment :
    Decodable (Builder FragmentDefinition) a
    -> List Directive
    -> Decodable (Builder Spec) (Maybe a -> b)
    -> Decodable (Builder Spec) b
withFragment decodableFragmentDefinition directives decodableParentSpec =
    decodableParentSpec
        |> andMap (fragmentSpread decodableFragmentDefinition directives)


inlineFragment :
    Maybe String
    -> List Directive
    -> Decodable (Builder Spec) a
    -> Decodable (Builder Spec) (Maybe a)
inlineFragment typeCondition directives (Decodable (Builder specErrs spec) fragmentDecoder) =
    let
        inlineFragment =
            InlineFragment
                { typeCondition = typeCondition
                , directives = directives
                , spec = spec
                }

        spec =
            ObjectSpec [ InlineFragmentSelection inlineFragment ]

        decoder =
            Decode.maybe fragmentDecoder

        inlineFragmentErrs =
            case spec of
                ObjectSpec _ ->
                    []

                _ ->
                    [ InvalidFragment spec ]
    in
        Decodable (Builder (specErrs ++ inlineFragmentErrs) spec) decoder


withInlineFragment :
    Maybe String
    -> List Directive
    -> Decodable (Builder Spec) a
    -> Decodable (Builder Spec) (Maybe a -> b)
    -> Decodable (Builder Spec) b
withInlineFragment typeCondition directives decodableFragmentSpec decodableParentSpec =
    decodableParentSpec
        |> andMap (inlineFragment typeCondition directives decodableFragmentSpec)


fragment : String -> String -> List Directive -> Decodable (Builder Spec) a -> Decodable (Builder FragmentDefinition) a
fragment name typeCondition directives =
    mapNode
        (\(Builder specErrs spec) ->
            let
                fragmentDefinition =
                    FragmentDefinition
                        { name = name
                        , typeCondition = typeCondition
                        , directives = directives
                        , spec = spec
                        }

                fragmentErrs =
                    case spec of
                        ObjectSpec _ ->
                            []

                        _ ->
                            [ InvalidFragment spec ]
            in
                Builder (specErrs ++ fragmentErrs) fragmentDefinition
        )


query : List QueryOption -> Decodable (Builder Spec) a -> Decodable (Builder Query) a
query queryOptions =
    (mapNode << mapBuilder)
        (\spec ->
            (case spec of
                ObjectSpec selections ->
                    Query { name = Nothing, spec = spec }

                _ ->
                    Query { name = Nothing, spec = ObjectSpec [] }
            )
                |> flip (List.foldr applyQueryOption) queryOptions
        )
