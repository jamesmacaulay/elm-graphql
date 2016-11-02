module GraphQL.Query exposing (..)

import GraphQL.Query.Arg as Arg
import Json.Decode as Decode exposing (Decoder, (:=))


type Selection
    = FieldSelection Field
    | FragmentSpreadSelection FragmentSpread
    | InlineFragmentSelection InlineFragment


type SelectionSet
    = SelectionSet (List Selection)


type Spec
    = IntSpec
    | FloatSpec
    | StringSpec
    | BooleanSpec
    | ObjectSpec SelectionSet
    | ListSpec Spec
    | MaybeSpec Spec
    | InvalidSpec String Spec


getBaseSpec : Spec -> Spec
getBaseSpec spec =
    case spec of
        ListSpec inner ->
            inner

        MaybeSpec inner ->
            inner

        InvalidSpec _ inner ->
            inner

        _ ->
            spec


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
        , selectionSet : SelectionSet
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
        , selectionSet : SelectionSet
        }


type Query
    = Query
        { name : Maybe String
        , selectionSet : SelectionSet
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


string : Decodable Spec String
string =
    Decodable StringSpec Decode.string


int : Decodable Spec Int
int =
    Decodable IntSpec Decode.int


float : Decodable Spec Float
float =
    Decodable FloatSpec Decode.float


bool : Decodable Spec Bool
bool =
    Decodable BooleanSpec Decode.bool


list : Decodable Spec a -> Decodable Spec (List a)
list =
    mapDecodable ListSpec Decode.list


maybe : Decodable Spec a -> Decodable Spec (Maybe a)
maybe =
    let
        nullable decoder =
            Decode.oneOf [ Decode.map Just decoder, Decode.null Nothing ]
    in
        mapDecodable MaybeSpec nullable


object : (a -> b) -> Decodable Spec (a -> b)
object constructor =
    Decodable (ObjectSpec (SelectionSet [])) (Decode.succeed constructor)


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


addSelection : Selection -> List Selection -> List Selection
addSelection s selections =
    selections ++ [ s ]


withField :
    String
    -> List FieldOption
    -> Decodable Spec a
    -> Decodable Spec (a -> b)
    -> Decodable Spec b
withField name fieldOptions decodableFieldSpec decodableParentSpec =
    let
        (Decodable parentSpec parentDecoder) =
            decodableParentSpec

        (Decodable fieldSpec fieldValueDecoder) =
            decodableFieldSpec

        decoder =
            Decode.object2 (<|) parentDecoder (name := fieldValueDecoder)

        spec =
            case parentSpec of
                ObjectSpec (SelectionSet selections) ->
                    let
                        field =
                            Field
                                { name = name
                                , spec = fieldSpec
                                , fieldAlias = Nothing
                                , args = []
                                , directives = []
                                }
                                |> flip (List.foldr applyFieldOption) fieldOptions

                        selections =
                            addSelection (FieldSelection field) selections
                    in
                        ObjectSpec (SelectionSet selections)

                _ ->
                    parentSpec
                        |> InvalidSpec ("Tried to add field " ++ toString name ++ " to a non-object: " ++ toString parentSpec)
    in
        Decodable spec decoder


extractField : String -> List FieldOption -> Decodable Spec a -> Decodable Spec a
extractField name options child =
    object identity
        |> withField name options child


withFragment :
    Decodable FragmentDefinition a
    -> List Directive
    -> Decodable Spec (Maybe a -> b)
    -> Decodable Spec b
withFragment decodableFragmentDefinition directives decodableSelectionSet =
    let
        (Decodable (FragmentDefinition fragmentDefinition) fragmentDecoder) =
            decodableFragmentDefinition

        (Decodable parentSpec parentDecoder) =
            decodableSelectionSet

        decoder =
            Decode.object2 (<|) parentDecoder (Decode.maybe fragmentDecoder)

        spec =
            case parentSpec of
                ObjectSpec (SelectionSet selections) ->
                    let
                        fragmentSpread =
                            FragmentSpread
                                { name = fragmentDefinition.name
                                , directives = directives
                                }

                        selections =
                            addSelection (FragmentSpreadSelection fragmentSpread) selections
                    in
                        ObjectSpec (SelectionSet selections)

                _ ->
                    parentSpec
                        |> InvalidSpec ("Tried to add fragment " ++ toString fragmentDefinition.name ++ " to a non-object: " ++ toString parentSpec)
    in
        Decodable spec decoder


withInlineFragment :
    Maybe String
    -> List Directive
    -> Decodable SelectionSet a
    -> Decodable Spec (Maybe a -> b)
    -> Decodable Spec b
withInlineFragment typeCondition directives decodableFragmentSelectionSet decodableParentSpec =
    let
        (Decodable fragmentSelectionSet fragmentDecoder) =
            decodableFragmentSelectionSet

        (Decodable parentSpec objectDecoder) =
            decodableParentSpec

        decoder =
            Decode.object2 (<|) objectDecoder (Decode.maybe fragmentDecoder)

        spec =
            case parentSpec of
                ObjectSpec (SelectionSet selections) ->
                    let
                        inlineFragment =
                            InlineFragment
                                { typeCondition = typeCondition
                                , directives = directives
                                , selectionSet = fragmentSelectionSet
                                }

                        selections =
                            addSelection (InlineFragmentSelection inlineFragment) selections
                    in
                        ObjectSpec (SelectionSet selections)

                _ ->
                    parentSpec
                        |> InvalidSpec ("Tried to add inline fragment " ++ toString decodableFragmentSelectionSet ++ " to a non-object: " ++ toString parentSpec)
    in
        Decodable spec decoder


fragment : String -> String -> List Directive -> Decodable SelectionSet a -> Decodable FragmentDefinition a
fragment name typeCondition directives =
    mapNode
        (\selectionSet ->
            FragmentDefinition
                { name = name
                , typeCondition = typeCondition
                , directives = directives
                , selectionSet = selectionSet
                }
        )


query : List QueryOption -> Decodable Spec a -> Decodable Query a
query queryOptions =
    mapNode
        (\spec ->
            (case spec of
                ObjectSpec selectionSet ->
                    Query { name = Nothing, selectionSet = selectionSet }

                _ ->
                    Query { name = Nothing, selectionSet = (SelectionSet []) }
            )
                |> flip (List.foldr applyQueryOption) queryOptions
        )
