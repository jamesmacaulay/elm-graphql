module GraphQL.Query.Builder
    exposing
        ( BuilderError
        , Spec
        , FragmentDefinition
        , Op
        , getStructure
        , getDecoder
        , string
        , int
        , float
        , bool
        , list
        , nullable
        , produce
        , fieldAlias
        , fieldArgs
        , fieldDirective
        , opName
        , opVariable
        , opVariableWithDefault
        , opDirective
        , map
        , andMap
        , field
        , withField
        , fragmentSpread
        , withFragment
        , inlineFragment
        , withInlineFragment
        , fragment
        , query
        , mutation
        )

import GraphQL.Query.Builder.Structure as Structure
import GraphQL.Query.Builder.Arg as Arg
import Json.Decode as Decode exposing (Decoder)


type alias BuilderError =
    Structure.BuilderError


type alias Spec a =
    Decodable (Structure.Builder Structure.Spec) a


type alias FragmentDefinition a =
    Decodable (Structure.Builder Structure.FragmentDefinition) a


type alias Op a =
    Decodable (Structure.Builder Structure.Op) a


type Decodable node result
    = Decodable node (Decoder result)


mapDecodable : (a -> b) -> (Decoder c -> Decoder d) -> Decodable a c -> Decodable b d
mapDecodable f g (Decodable node decoder) =
    Decodable (f node) (g decoder)


mapStructure : (a -> b) -> Decodable a result -> Decodable b result
mapStructure f =
    mapDecodable f identity


mapDecoder : (Decoder a -> Decoder b) -> Decodable node a -> Decodable node b
mapDecoder =
    mapDecodable identity


getStructure : Decodable node result -> node
getStructure (Decodable node _) =
    node


getDecoder : Decodable a result -> Decoder result
getDecoder (Decodable _ decoder) =
    decoder


string : Spec String
string =
    Decodable Structure.string Decode.string


int : Spec Int
int =
    Decodable Structure.int Decode.int


float : Spec Float
float =
    Decodable Structure.float Decode.float


bool : Spec Bool
bool =
    Decodable Structure.bool Decode.bool


list : Spec a -> Spec (List a)
list =
    mapDecodable Structure.list Decode.list


nullableDecoder : Decoder a -> Decoder (Maybe a)
nullableDecoder decoder =
    Decode.oneOf [ Decode.map Just decoder, Decode.null Nothing ]


nullable : Spec a -> Spec (Maybe a)
nullable =
    mapDecodable Structure.nullable nullableDecoder


produce : a -> Spec a
produce x =
    Decodable Structure.any (Decode.succeed x)


fieldAlias : String -> Structure.FieldOption
fieldAlias =
    Structure.fieldAlias


fieldArgs : List ( String, Arg.Value ) -> Structure.FieldOption
fieldArgs =
    Structure.fieldArgs


fieldDirective : String -> List ( String, Arg.Value ) -> Structure.FieldOption
fieldDirective =
    Structure.fieldDirective


opName : String -> Structure.OpOption
opName =
    Structure.opName


opVariable : String -> String -> Structure.OpOption
opVariable =
    Structure.opVariable


opVariableWithDefault : String -> String -> Arg.Value -> Structure.OpOption
opVariableWithDefault =
    Structure.opVariableWithDefault


opDirective : String -> List ( String, Arg.Value ) -> Structure.OpOption
opDirective =
    Structure.opDirective


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
            Structure.field name fieldOptions valueSpec

        spec =
            Structure.ObjectSpec [ Structure.FieldSelection field ]

        decoder =
            Decode.field (Structure.responseKey field) valueDecoder
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
    mapStructure
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


query : List Structure.OpOption -> Spec a -> Op a
query opOptions =
    Structure.query opOptions
        |> Structure.mapBuilder
        |> mapStructure


mutation : List Structure.OpOption -> Spec a -> Op a
mutation opOptions =
    Structure.mutation opOptions
        |> Structure.mapBuilder
        |> mapStructure
