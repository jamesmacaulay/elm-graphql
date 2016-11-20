module GraphQL.Query exposing (..)

import GraphQL.Query.Builder.Structure as Structure
import GraphQL.Query.Arg as Arg
import Json.Decode as Decode exposing (Decoder)


type BuilderError
    = InvalidIntersection Structure.Spec Structure.Spec
    | InvalidFragment Structure.Spec



-- specToSummaryString : Structure.Spec -> String
-- specToSummaryString spec =
--     case spec of
--         Structure.AnySpec ->
--             "any"
--         Structure.IntSpec ->
--             "int"
--         Structure.FloatSpec ->
--             "float"
--         Structure.StringSpec ->
--             "string"
--         Structure.BooleanSpec ->
--             "bool"
--         Structure.ObjectSpec selections ->
--             "object with fields " ++ selectionsToSummaryString selections
--         Structure.ListSpec inner ->
--             "list of " ++ specToSummaryString inner


type Builder a
    = Builder (List BuilderError) a


type alias Spec a =
    Decodable (Builder Structure.Spec) a


type alias FragmentDefinition a =
    Decodable (Builder Structure.FragmentDefinition) a


type alias Query a =
    Decodable (Builder Structure.Query) a


mapBuilder : (a -> b) -> Builder a -> Builder b
mapBuilder f (Builder errs spec) =
    Builder errs (f spec)


appendSelections : List Structure.Selection -> List Structure.Selection -> List Structure.Selection
appendSelections a b =
    a ++ b


specIntersection : Structure.Spec -> Structure.Spec -> Result BuilderError Structure.Spec
specIntersection a b =
    case ( a, b ) of
        ( Structure.AnySpec, _ ) ->
            Ok b

        ( _, Structure.AnySpec ) ->
            Ok a

        ( Structure.ObjectSpec ssa, Structure.ObjectSpec ssb ) ->
            Ok (Structure.ObjectSpec (appendSelections ssa ssb))

        ( Structure.ListSpec innerA, Structure.ListSpec innerB ) ->
            specIntersection innerA innerB
                |> Result.map Structure.ListSpec

        ( Structure.MaybeSpec innerA, Structure.MaybeSpec innerB ) ->
            specIntersection innerA innerB
                |> Result.map Structure.MaybeSpec

        ( Structure.IntSpec, Structure.IntSpec ) ->
            Ok Structure.IntSpec

        ( Structure.FloatSpec, Structure.FloatSpec ) ->
            Ok Structure.FloatSpec

        ( Structure.StringSpec, Structure.StringSpec ) ->
            Ok Structure.StringSpec

        ( Structure.BooleanSpec, Structure.BooleanSpec ) ->
            Ok Structure.BooleanSpec

        _ ->
            Err (InvalidIntersection a b)


specBuilderIntersection : Builder Structure.Spec -> Builder Structure.Spec -> Builder Structure.Spec
specBuilderIntersection (Builder errsA specA) (Builder errsB specB) =
    case specIntersection specA specB of
        Ok spec ->
            Builder (errsA ++ errsB) spec

        Err builderError ->
            Builder (errsA ++ errsB ++ [ builderError ]) specA


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
    Decodable (Builder [] Structure.StringSpec) Decode.string


int : Spec Int
int =
    Decodable (Builder [] Structure.IntSpec) Decode.int


float : Spec Float
float =
    Decodable (Builder [] Structure.FloatSpec) Decode.float


bool : Spec Bool
bool =
    Decodable (Builder [] Structure.BooleanSpec) Decode.bool


list : Spec a -> Spec (List a)
list =
    mapDecodable (mapBuilder Structure.ListSpec) Decode.list


maybe : Spec a -> Spec (Maybe a)
maybe =
    let
        nullable decoder =
            Decode.oneOf [ Decode.map Just decoder, Decode.null Nothing ]
    in
        mapDecodable (mapBuilder Structure.MaybeSpec) nullable


construct : (a -> b) -> Spec (a -> b)
construct constructor =
    Decodable (Builder [] Structure.AnySpec) (Decode.succeed constructor)


object : (a -> b) -> Spec (a -> b)
object constructor =
    Decodable (Builder [] (Structure.ObjectSpec [])) (Decode.succeed constructor)


fieldAlias : String -> Structure.FieldOption
fieldAlias =
    Structure.FieldAlias


fieldArgs : List ( String, Arg.Value ) -> Structure.FieldOption
fieldArgs =
    Structure.FieldArgs


applyFieldOption : Structure.FieldOption -> Structure.Field -> Structure.Field
applyFieldOption fieldOption field =
    case fieldOption of
        Structure.FieldAlias name ->
            { field | fieldAlias = Just name }

        Structure.FieldArgs args ->
            { field | args = field.args ++ args }

        Structure.FieldDirectives directives ->
            { field | directives = field.directives ++ directives }


queryName : String -> Structure.QueryOption
queryName =
    Structure.QueryName


applyQueryOption : Structure.QueryOption -> Structure.Query -> Structure.Query
applyQueryOption queryOption query =
    case queryOption of
        Structure.QueryName name ->
            { query | name = Just name }


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


field : String -> List Structure.FieldOption -> Spec a -> Spec a
field name fieldOptions (Decodable (Builder valueErrs valueSpec) valueDecoder) =
    let
        field =
            { name = name
            , spec = valueSpec
            , fieldAlias = Nothing
            , args = []
            , directives = []
            }
                |> flip (List.foldr applyFieldOption) fieldOptions

        spec =
            (Structure.ObjectSpec [ Structure.FieldSelection field ])

        decoder =
            (Decode.field name valueDecoder)
    in
        Decodable (Builder valueErrs spec) decoder


withField :
    String
    -> List Structure.FieldOption
    -> Spec a
    -> Spec (a -> b)
    -> Spec b
withField name fieldOptions decodableFieldSpec decodableParentSpec =
    decodableParentSpec
        |> andMap (field name fieldOptions decodableFieldSpec)


extractField : String -> List Structure.FieldOption -> Spec a -> Spec a
extractField =
    field


fragmentSpread : FragmentDefinition a -> List Structure.Directive -> Spec (Maybe a)
fragmentSpread (Decodable (Builder fragmentErrs { name }) fragmentDecoder) directives =
    let
        fragmentSpread =
            { name = name
            , directives = directives
            }

        spec =
            Structure.ObjectSpec [ Structure.FragmentSpreadSelection fragmentSpread ]

        decoder =
            Decode.maybe fragmentDecoder
    in
        Decodable (Builder fragmentErrs spec) decoder


withFragment :
    FragmentDefinition a
    -> List Structure.Directive
    -> Spec (Maybe a -> b)
    -> Spec b
withFragment decodableFragmentDefinition directives decodableParentSpec =
    decodableParentSpec
        |> andMap (fragmentSpread decodableFragmentDefinition directives)


inlineFragment :
    Maybe String
    -> List Structure.Directive
    -> Spec a
    -> Spec (Maybe a)
inlineFragment typeCondition directives (Decodable (Builder specErrs spec) fragmentDecoder) =
    let
        inlineFragment_ =
            { typeCondition = typeCondition
            , directives = directives
            , spec = spec
            }

        spec_ =
            Structure.ObjectSpec [ Structure.InlineFragmentSelection inlineFragment_ ]

        decoder =
            Decode.maybe fragmentDecoder

        inlineFragmentErrs =
            case spec_ of
                Structure.ObjectSpec _ ->
                    []

                _ ->
                    [ InvalidFragment spec_ ]
    in
        Decodable (Builder (specErrs ++ inlineFragmentErrs) spec_) decoder


withInlineFragment :
    Maybe String
    -> List Structure.Directive
    -> Spec a
    -> Spec (Maybe a -> b)
    -> Spec b
withInlineFragment typeCondition directives decodableFragmentSpec decodableParentSpec =
    decodableParentSpec
        |> andMap (inlineFragment typeCondition directives decodableFragmentSpec)


fragment : String -> String -> List Structure.Directive -> Spec a -> FragmentDefinition a
fragment name typeCondition directives =
    mapNode
        (\(Builder specErrs spec) ->
            let
                fragmentDefinition =
                    { name = name
                    , typeCondition = typeCondition
                    , directives = directives
                    , spec = spec
                    }

                fragmentErrs =
                    case spec of
                        Structure.ObjectSpec _ ->
                            []

                        _ ->
                            [ InvalidFragment spec ]
            in
                Builder (specErrs ++ fragmentErrs) fragmentDefinition
        )


query : List Structure.QueryOption -> Spec a -> Query a
query queryOptions =
    (mapNode << mapBuilder)
        (\spec ->
            (case spec of
                Structure.ObjectSpec selections ->
                    { name = Nothing, spec = spec }

                _ ->
                    { name = Nothing, spec = Structure.ObjectSpec [] }
            )
                |> flip (List.foldr applyQueryOption) queryOptions
        )
