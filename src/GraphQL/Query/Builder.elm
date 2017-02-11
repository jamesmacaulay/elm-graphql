module GraphQL.Query.Builder
    exposing
        ( Spec
        , FragmentDefinition
        , Query
        , Mutation
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
        , queryName
        , queryVariable
        , queryVariableWithDefault
        , queryDirective
        , mutationName
        , mutationVariable
        , mutationVariableWithDefault
        , mutationDirective
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


type alias Spec a =
    Decodable Structure.Spec a


type alias FragmentDefinition a =
    Decodable Structure.FragmentDefinition a


type alias Query a =
    Decodable Structure.Query a


type alias Mutation a =
    Decodable Structure.Mutation a


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


queryName : String -> Structure.QueryOption
queryName =
    Structure.queryName


queryVariable : String -> String -> Structure.QueryOption
queryVariable =
    Structure.queryVariable


queryVariableWithDefault : String -> String -> Arg.Value -> Structure.QueryOption
queryVariableWithDefault =
    Structure.queryVariableWithDefault


queryDirective : String -> List ( String, Arg.Value ) -> Structure.QueryOption
queryDirective =
    Structure.queryDirective


mutationName : String -> Structure.MutationOption
mutationName =
    Structure.mutationName


mutationVariable : String -> String -> Structure.MutationOption
mutationVariable =
    Structure.mutationVariable


mutationVariableWithDefault : String -> String -> Arg.Value -> Structure.MutationOption
mutationVariableWithDefault =
    Structure.mutationVariableWithDefault


mutationDirective : String -> List ( String, Arg.Value ) -> Structure.MutationOption
mutationDirective =
    Structure.mutationDirective


map : (a -> b) -> Spec a -> Spec b
map f =
    mapDecoder (Decode.map f)


andMap : Spec a -> Spec (a -> b) -> Spec b
andMap (Decodable littleSpec littleDecoder) (Decodable bigSpec bigDecoder) =
    let
        spec =
            Structure.join bigSpec littleSpec

        decoder =
            Decode.map2 (<|) bigDecoder littleDecoder
    in
        Decodable spec decoder


field : String -> List Structure.FieldOption -> Spec a -> Spec a
field name fieldOptions (Decodable valueSpec valueDecoder) =
    let
        field =
            Structure.field name fieldOptions valueSpec

        spec =
            Structure.ObjectSpec [ Structure.FieldSelection field ]

        decoder =
            Decode.field (Structure.responseKey field) valueDecoder
    in
        Decodable spec decoder


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
fragmentSpread (Decodable { name } fragmentDecoder) directives =
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
        Decodable spec decoder


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
inlineFragment typeCondition directives (Decodable fragmentSpec fragmentDecoder) =
    let
        inlineFragment =
            { typeCondition = typeCondition
            , directives = directives
            , spec = fragmentSpec
            }

        spec =
            Structure.ObjectSpec [ Structure.InlineFragmentSelection inlineFragment ]

        decoder =
            Decode.maybe fragmentDecoder
    in
        Decodable spec decoder


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
fragment name typeCondition directives spec =
    spec
        |> mapStructure (Structure.FragmentDefinition name typeCondition directives)


query : List Structure.QueryOption -> Spec a -> Query a
query opOptions spec =
    spec
        |> mapStructure (Structure.query opOptions)


mutation : List Structure.MutationOption -> Spec a -> Mutation a
mutation opOptions spec =
    spec
        |> mapStructure (Structure.mutation opOptions)
