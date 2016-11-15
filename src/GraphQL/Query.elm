module GraphQL.Query exposing (..)

import GraphQL.Query.Arg as Arg
import Json.Decode as Decode exposing (Decoder)


type Selection
    = FieldSelection Field
    | FragmentSpreadSelection FragmentSpread
    | InlineFragmentSelection InlineFragment


type BuilderError
    = InvalidIntersection SpecStructure SpecStructure
    | InvalidFragment SpecStructure


type SpecStructure
    = AnySpec
    | IntSpec
    | FloatSpec
    | StringSpec
    | BooleanSpec
    | ObjectSpec (List Selection)
    | ListSpec SpecStructure
    | MaybeSpec SpecStructure


getBaseSpec : SpecStructure -> SpecStructure
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


type alias Spec a =
    Decodable (Builder SpecStructure) a


type alias FragmentDefinition a =
    Decodable (Builder FragmentDefinitionStructure) a


type alias Query a =
    Decodable (Builder QueryStructure) a


mapBuilder : (a -> b) -> Builder a -> Builder b
mapBuilder f (Builder errs spec) =
    Builder errs (f spec)


appendSelections : List Selection -> List Selection -> List Selection
appendSelections a b =
    a ++ b


specIntersection : SpecStructure -> SpecStructure -> Result BuilderError SpecStructure
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


specBuilderIntersection : Builder SpecStructure -> Builder SpecStructure -> Builder SpecStructure
specBuilderIntersection (Builder errsA specA) (Builder errsB specB) =
    case specIntersection specA specB of
        Ok spec ->
            Builder (errsA ++ errsB) spec

        Err builderError ->
            Builder (errsA ++ errsB ++ [ builderError ]) specA


type Field
    = Field
        { name : String
        , spec : SpecStructure
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


type FragmentDefinitionStructure
    = FragmentDefinitionStructure
        { name : String
        , typeCondition : String
        , spec : SpecStructure
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
        , spec : SpecStructure
        }


type QueryStructure
    = QueryStructure
        { name : Maybe String
        , spec : SpecStructure
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


string : Spec String
string =
    Decodable (Builder [] StringSpec) Decode.string


int : Spec Int
int =
    Decodable (Builder [] IntSpec) Decode.int


float : Spec Float
float =
    Decodable (Builder [] FloatSpec) Decode.float


bool : Spec Bool
bool =
    Decodable (Builder [] BooleanSpec) Decode.bool


list : Spec a -> Spec (List a)
list =
    mapDecodable (mapBuilder ListSpec) Decode.list


maybe : Spec a -> Spec (Maybe a)
maybe =
    let
        nullable decoder =
            Decode.oneOf [ Decode.map Just decoder, Decode.null Nothing ]
    in
        mapDecodable (mapBuilder MaybeSpec) nullable


construct : (a -> b) -> Spec (a -> b)
construct constructor =
    Decodable (Builder [] AnySpec) (Decode.succeed constructor)


object : (a -> b) -> Spec (a -> b)
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


applyQueryOption : QueryOption -> QueryStructure -> QueryStructure
applyQueryOption queryOption (QueryStructure queryInfo) =
    case queryOption of
        QueryName name ->
            QueryStructure { queryInfo | name = Just name }


map : (a -> b) -> Spec a -> Spec b
map f =
    mapDecoder (Decode.map f)


andMap : Spec a -> Spec (a -> b) -> Spec b
andMap (Decodable littleSpec littleDecoder) (Decodable bigSpec bigDecoder) =
    let
        specBuilder =
            specBuilderIntersection bigSpec littleSpec

        decoder =
            Decode.map2 (<|) bigDecoder littleDecoder
    in
        Decodable specBuilder decoder


field : String -> List FieldOption -> Spec a -> Spec a
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
            (Decode.field name valueDecoder)
    in
        Decodable (Builder valueErrs spec) decoder


withField :
    String
    -> List FieldOption
    -> Spec a
    -> Spec (a -> b)
    -> Spec b
withField name fieldOptions decodableFieldSpec decodableParentSpec =
    decodableParentSpec
        |> andMap (field name fieldOptions decodableFieldSpec)


extractField : String -> List FieldOption -> Spec a -> Spec a
extractField =
    field


fragmentSpread : FragmentDefinition a -> List Directive -> Spec (Maybe a)
fragmentSpread (Decodable (Builder fragmentErrs (FragmentDefinitionStructure { name })) fragmentDecoder) directives =
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
    FragmentDefinition a
    -> List Directive
    -> Spec (Maybe a -> b)
    -> Spec b
withFragment decodableFragmentDefinition directives decodableParentSpec =
    decodableParentSpec
        |> andMap (fragmentSpread decodableFragmentDefinition directives)


inlineFragment :
    Maybe String
    -> List Directive
    -> Spec a
    -> Spec (Maybe a)
inlineFragment typeCondition directives (Decodable (Builder specErrs spec) fragmentDecoder) =
    let
        inlineFragment_ =
            InlineFragment
                { typeCondition = typeCondition
                , directives = directives
                , spec = spec
                }

        spec_ =
            ObjectSpec [ InlineFragmentSelection inlineFragment_ ]

        decoder =
            Decode.maybe fragmentDecoder

        inlineFragmentErrs =
            case spec_ of
                ObjectSpec _ ->
                    []

                _ ->
                    [ InvalidFragment spec_ ]
    in
        Decodable (Builder (specErrs ++ inlineFragmentErrs) spec_) decoder


withInlineFragment :
    Maybe String
    -> List Directive
    -> Spec a
    -> Spec (Maybe a -> b)
    -> Spec b
withInlineFragment typeCondition directives decodableFragmentSpec decodableParentSpec =
    decodableParentSpec
        |> andMap (inlineFragment typeCondition directives decodableFragmentSpec)


fragment : String -> String -> List Directive -> Spec a -> FragmentDefinition a
fragment name typeCondition directives =
    mapNode
        (\(Builder specErrs spec) ->
            let
                fragmentDefinition =
                    FragmentDefinitionStructure
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


query : List QueryOption -> Spec a -> Query a
query queryOptions =
    (mapNode << mapBuilder)
        (\spec ->
            (case spec of
                ObjectSpec selections ->
                    QueryStructure { name = Nothing, spec = spec }

                _ ->
                    QueryStructure { name = Nothing, spec = ObjectSpec [] }
            )
                |> flip (List.foldr applyQueryOption) queryOptions
        )
