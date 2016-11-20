module GraphQL.Query exposing (..)

import GraphQL.Query.Builder.Structure as Structure
import GraphQL.Query.Arg as Arg
import Json.Decode as Decode exposing (Decoder)


type alias Spec a =
    Decodable (Structure.Builder Structure.Spec) a


type alias FragmentDefinition a =
    Decodable (Structure.Builder Structure.FragmentDefinition) a


type alias Query a =
    Decodable (Structure.Builder Structure.Query) a


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
    Decodable (Structure.Builder [] Structure.StringSpec) Decode.string


int : Spec Int
int =
    Decodable (Structure.Builder [] Structure.IntSpec) Decode.int


float : Spec Float
float =
    Decodable (Structure.Builder [] Structure.FloatSpec) Decode.float


bool : Spec Bool
bool =
    Decodable (Structure.Builder [] Structure.BooleanSpec) Decode.bool


list : Spec a -> Spec (List a)
list =
    mapDecodable (Structure.mapBuilder Structure.ListSpec) Decode.list


maybe : Spec a -> Spec (Maybe a)
maybe =
    let
        nullable decoder =
            Decode.oneOf [ Decode.map Just decoder, Decode.null Nothing ]
    in
        mapDecodable (Structure.mapBuilder Structure.MaybeSpec) nullable


construct : (a -> b) -> Spec (a -> b)
construct constructor =
    Decodable (Structure.Builder [] Structure.AnySpec) (Decode.succeed constructor)


object : (a -> b) -> Spec (a -> b)
object constructor =
    Decodable (Structure.Builder [] (Structure.ObjectSpec [])) (Decode.succeed constructor)


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
            Structure.specBuilderIntersection bigSpec littleSpec

        decoder =
            Decode.map2 (<|) bigDecoder littleDecoder
    in
        Decodable specBuilder decoder


field : String -> List Structure.FieldOption -> Spec a -> Spec a
field name fieldOptions (Decodable (Structure.Builder valueErrs valueSpec) valueDecoder) =
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
        Decodable (Structure.Builder valueErrs spec) decoder


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
fragmentSpread (Decodable (Structure.Builder fragmentErrs { name }) fragmentDecoder) directives =
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
        Decodable (Structure.Builder fragmentErrs spec) decoder


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
inlineFragment typeCondition directives (Decodable (Structure.Builder specErrs spec) fragmentDecoder) =
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
                    [ Structure.InvalidFragment spec_ ]
    in
        Decodable (Structure.Builder (specErrs ++ inlineFragmentErrs) spec_) decoder


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
        (\(Structure.Builder specErrs spec) ->
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
                            [ Structure.InvalidFragment spec ]
            in
                Structure.Builder (specErrs ++ fragmentErrs) fragmentDefinition
        )


query : List Structure.QueryOption -> Spec a -> Query a
query queryOptions =
    (mapNode << Structure.mapBuilder)
        (\spec ->
            (case spec of
                Structure.ObjectSpec selections ->
                    { name = Nothing, spec = spec }

                _ ->
                    { name = Nothing, spec = Structure.ObjectSpec [] }
            )
                |> flip (List.foldr applyQueryOption) queryOptions
        )
