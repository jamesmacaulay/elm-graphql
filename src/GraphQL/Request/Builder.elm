module GraphQL.Request.Builder
    exposing
        ( Request
        , Operation
        , Query
        , Mutation
        , Fragment
        , Spec
        , IntType
        , FloatType
        , StringType
        , BooleanType
        , IdType
        , EnumType
        , ListType
        , ObjectType
        , Nullable
        , NonNull
        , request
        , query
        , queryOperationType
        , mutation
        , mutationOperationType
        , fragment
        , int
        , float
        , string
        , bool
        , id
        , enum
        , enumWithDefault
        , list
        , nullable
        , object
        , produce
        , withField
        , field
        , alias
        , args
        , directive
        , map
        , map2
        , map3
        , map4
        , map5
        , map6
        , map7
        , map8
        , andMap
        , requestAST
        , responseDecoder
        , specDecoder
        )

import Json.Decode as Decode exposing (Decoder)
import GraphQL.Request.Document.AST as AST
import GraphQL.Request.Document.AST.Util as Util
import GraphQL.Request.Builder.TypeRef as TypeRef
import GraphQL.Request.Builder.Value as Value
import GraphQL.Response as Response
import Dict exposing (Dict)
import Set exposing (Set)


type Spec nullability coreType result
    = Spec (SourceType nullability coreType) (AST.SelectionSet -> Decoder result)


type alias TypeCondition =
    AST.TypeCondition


type alias Fragment result =
    { name : String
    , typeCondition : TypeCondition
    , directives : List ( String, List ( String, Value.Argument ) )
    , spec : Spec NonNull ObjectType result
    }


type alias Request operationType result =
    { operation : Operation operationType result
    , variableValues : List ( String, Value.Constant )
    }


type alias Operation operationType result =
    { operationType : OperationType operationType
    , name : Maybe String
    , variableDefinitions : List ( String, TypeRef.TypeRef, Maybe Value.Constant )
    , directives : List ( String, List ( String, Value.Argument ) )
    , spec : Spec NonNull ObjectType result
    }


type OperationType operationType
    = QueryOperationType
    | MutationOperationType


type Query
    = Query


type Mutation
    = Mutation


type Nullable
    = Nullable


type NonNull
    = NonNull


type Nullability a
    = NullableFlag
    | NonNullFlag


nullableFlag : Nullability Nullable
nullableFlag =
    NullableFlag


nonNullFlag : Nullability NonNull
nonNullFlag =
    NonNullFlag


type SourceType nullability coreType
    = SpecifiedType (SpecifiedTypeInfo nullability coreType)
    | AnyType


type alias SpecifiedTypeInfo nullability coreType =
    { nullability : Nullability nullability
    , coreType : coreType
    , join : coreType -> coreType -> coreType
    , selectionSet : AST.SelectionSet
    }


type IntType
    = IntType


type FloatType
    = FloatType


type StringType
    = StringType


type BooleanType
    = BooleanType


type IdType
    = IdType


type EnumType
    = EnumType (List String)


type ListType itemNullability itemType
    = ListType (SourceType itemNullability itemType)


type ObjectType
    = ObjectType


type FieldOption
    = FieldAlias String
    | FieldArgs (List ( String, Value.Argument ))
    | FieldDirective String (List ( String, Value.Argument ))


request :
    List ( String, Value.Constant )
    -> Operation operationType result
    -> Request operationType result
request variableValues operation =
    Request operation variableValues


query :
    List ( String, TypeRef.TypeRef, Maybe Value.Constant )
    -> Spec NonNull ObjectType result
    -> Operation Query result
query variableDefinitions spec =
    { operationType = queryOperationType
    , name = Nothing
    , variableDefinitions = variableDefinitions
    , directives = []
    , spec = spec
    }


queryOperationType : OperationType Query
queryOperationType =
    QueryOperationType


mutation :
    List ( String, TypeRef.TypeRef, Maybe Value.Constant )
    -> Spec NonNull ObjectType result
    -> Operation Mutation result
mutation variableDefinitions spec =
    { operationType = mutationOperationType
    , name = Nothing
    , variableDefinitions = variableDefinitions
    , directives = []
    , spec = spec
    }


mutationOperationType : OperationType Mutation
mutationOperationType =
    MutationOperationType


fragment :
    String
    -> TypeCondition
    -> Spec NonNull ObjectType result
    -> Fragment result
fragment name typeCondition spec =
    { name = name
    , typeCondition = typeCondition
    , directives = []
    , spec = spec
    }


onType : String -> TypeCondition
onType =
    AST.TypeCondition


object :
    (fieldValue -> result)
    -> Spec NonNull ObjectType (fieldValue -> result)
object ctr =
    Spec emptyObjectSpecifiedType (always (Decode.succeed ctr))


withField :
    String
    -> List FieldOption
    -> Spec nullability coreType a
    -> Spec NonNull ObjectType (a -> b)
    -> Spec NonNull ObjectType b
withField name fieldOptions spec fSpec =
    fSpec
        |> andMap (field name fieldOptions spec)


field :
    String
    -> List FieldOption
    -> Spec nullability coreType result
    -> Spec NonNull ObjectType result
field name fieldOptions (Spec sourceType fieldDecoder) =
    let
        astFieldInfo =
            fieldOptions
                |> List.foldl applyFieldOption
                    { alias = Nothing
                    , name = name
                    , arguments = []
                    , directives = []
                    , selectionSet = selectionSetFromSourceType sourceType
                    }

        selectionSet =
            AST.SelectionSet [ AST.Field astFieldInfo ]

        decoder selectionSet =
            let
                fieldInSelectionSet =
                    astFieldInfo

                responseKey =
                    Util.responseKey fieldInSelectionSet
            in
                Decode.field responseKey (fieldDecoder fieldInSelectionSet.selectionSet)
    in
        Spec
            (SpecifiedType
                { nullability = nonNullFlag
                , coreType = ObjectType
                , join = always
                , selectionSet = selectionSet
                }
            )
            decoder


alias : String -> FieldOption
alias =
    FieldAlias


args : List ( String, Value.Argument ) -> FieldOption
args =
    FieldArgs


directive : String -> List ( String, Value.Argument ) -> FieldOption
directive =
    FieldDirective


int : Spec NonNull IntType Int
int =
    primitiveSpec IntType Decode.int


float : Spec NonNull FloatType Float
float =
    primitiveSpec FloatType Decode.float


string : Spec NonNull StringType String
string =
    primitiveSpec StringType Decode.string


bool : Spec NonNull BooleanType Bool
bool =
    primitiveSpec BooleanType Decode.bool


id : Spec NonNull IdType String
id =
    primitiveSpec IdType Decode.string


enum : List ( String, a ) -> Spec NonNull EnumType a
enum =
    enumWithFallback
        (\label ->
            Decode.fail ("Unexpected enum value " ++ toString label)
        )


enumWithDefault : (String -> a) -> List ( String, a ) -> Spec NonNull EnumType a
enumWithDefault ctr =
    enumWithFallback
        (\label ->
            Decode.succeed (ctr label)
        )


enumWithFallback : (String -> Decoder a) -> List ( String, a ) -> Spec NonNull EnumType a
enumWithFallback fallbackDecoder labelledValues =
    let
        decoderFromLabel =
            decoderFromEnumLabel fallbackDecoder labelledValues

        decoder =
            Decode.string
                |> Decode.andThen decoderFromLabel

        labels =
            List.map Tuple.first labelledValues
    in
        Spec
            (SpecifiedType
                { nullability = nonNullFlag
                , coreType = EnumType labels
                , join = enumJoin
                , selectionSet = emptySelectionSet
                }
            )
            (always decoder)


decoderFromEnumLabel : (String -> Decoder a) -> List ( String, a ) -> String -> Decoder a
decoderFromEnumLabel fallbackDecoder labelledValues =
    let
        valueFromLabel =
            flip Dict.get (Dict.fromList labelledValues)

        decoder enumString =
            case valueFromLabel enumString of
                Just value ->
                    Decode.succeed value

                Nothing ->
                    fallbackDecoder enumString
    in
        decoder


list :
    Spec itemNullability itemType result
    -> Spec NonNull (ListType itemNullability itemType) (List result)
list (Spec itemType decoder) =
    Spec
        (SpecifiedType
            { nullability = nonNullFlag
            , coreType = (ListType itemType)
            , join = listJoin
            , selectionSet = selectionSetFromSourceType itemType
            }
        )
        (Decode.list << decoder)


nullable : Spec NonNull coreType result -> Spec Nullable coreType (Maybe result)
nullable (Spec sourceType decoder) =
    case sourceType of
        SpecifiedType typeInfo ->
            Spec
                (SpecifiedType { typeInfo | nullability = nullableFlag })
                (Decode.nullable << decoder)

        AnyType ->
            Spec AnyType (Decode.nullable << decoder)


emptyObjectSpecifiedType : SourceType NonNull ObjectType
emptyObjectSpecifiedType =
    SpecifiedType
        { nullability = nonNullFlag
        , coreType = ObjectType
        , join = always
        , selectionSet = emptySelectionSet
        }


produce : result -> Spec nullability coreType result
produce x =
    Spec AnyType (always (Decode.succeed x))


map : (a -> b) -> Spec nullability coreType a -> Spec nullability coreType b
map f (Spec sourceType decoder) =
    Spec sourceType (decoder >> Decode.map f)


map2 :
    (a -> b -> c)
    -> Spec nullability coreType a
    -> Spec nullability coreType b
    -> Spec nullability coreType c
map2 f (Spec sourceTypeA decoderA) (Spec sourceTypeB decoderB) =
    let
        joinedSourceType =
            join sourceTypeA sourceTypeB

        joinedDecoder selectionSet =
            Decode.map2 f (decoderA selectionSet) (decoderB selectionSet)
    in
        Spec joinedSourceType joinedDecoder


map3 :
    (a -> b -> c -> d)
    -> Spec nullability coreType a
    -> Spec nullability coreType b
    -> Spec nullability coreType c
    -> Spec nullability coreType d
map3 f s1 s2 s3 =
    map f s1
        |> andMap s2
        |> andMap s3


map4 :
    (a -> b -> c -> d -> e)
    -> Spec nullability coreType a
    -> Spec nullability coreType b
    -> Spec nullability coreType c
    -> Spec nullability coreType d
    -> Spec nullability coreType e
map4 f s1 s2 s3 s4 =
    map f s1
        |> andMap s2
        |> andMap s3
        |> andMap s4


map5 :
    (a -> b -> c -> d -> e -> f)
    -> Spec nullability coreType a
    -> Spec nullability coreType b
    -> Spec nullability coreType c
    -> Spec nullability coreType d
    -> Spec nullability coreType e
    -> Spec nullability coreType f
map5 f s1 s2 s3 s4 s5 =
    map f s1
        |> andMap s2
        |> andMap s3
        |> andMap s4
        |> andMap s5


map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Spec nullability coreType a
    -> Spec nullability coreType b
    -> Spec nullability coreType c
    -> Spec nullability coreType d
    -> Spec nullability coreType e
    -> Spec nullability coreType f
    -> Spec nullability coreType g
map6 f s1 s2 s3 s4 s5 s6 =
    map f s1
        |> andMap s2
        |> andMap s3
        |> andMap s4
        |> andMap s5
        |> andMap s6


map7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> Spec nullability coreType a
    -> Spec nullability coreType b
    -> Spec nullability coreType c
    -> Spec nullability coreType d
    -> Spec nullability coreType e
    -> Spec nullability coreType f
    -> Spec nullability coreType g
    -> Spec nullability coreType h
map7 f s1 s2 s3 s4 s5 s6 s7 =
    map f s1
        |> andMap s2
        |> andMap s3
        |> andMap s4
        |> andMap s5
        |> andMap s6
        |> andMap s7


map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i)
    -> Spec nullability coreType a
    -> Spec nullability coreType b
    -> Spec nullability coreType c
    -> Spec nullability coreType d
    -> Spec nullability coreType e
    -> Spec nullability coreType f
    -> Spec nullability coreType g
    -> Spec nullability coreType h
    -> Spec nullability coreType i
map8 f s1 s2 s3 s4 s5 s6 s7 s8 =
    map f s1
        |> andMap s2
        |> andMap s3
        |> andMap s4
        |> andMap s5
        |> andMap s6
        |> andMap s7
        |> andMap s8


andMap : Spec nullability coreType a -> Spec nullability coreType (a -> b) -> Spec nullability coreType b
andMap specA specF =
    map2 (<|) specF specA


applyFieldOption : FieldOption -> AST.FieldInfo -> AST.FieldInfo
applyFieldOption fieldOption field =
    case fieldOption of
        FieldAlias name ->
            { field | alias = Just name }

        FieldArgs arguments ->
            { field | arguments = field.arguments ++ arguments }

        FieldDirective name arguments ->
            { field
                | directives =
                    field.directives
                        ++ [ AST.Directive
                                { name = name
                                , arguments = arguments
                                }
                           ]
            }


enumJoin : EnumType -> EnumType -> EnumType
enumJoin (EnumType labelsA) (EnumType labelsB) =
    Set.fromList labelsA
        |> Set.intersect (Set.fromList labelsB)
        |> Set.toList
        |> EnumType


listJoin :
    ListType itemNullability itemType
    -> ListType itemNullability itemType
    -> ListType itemNullability itemType
listJoin (ListType itemSourceTypeA) (ListType itemSourceTypeB) =
    ListType (join itemSourceTypeA itemSourceTypeB)


mergeSelectionSets : AST.SelectionSet -> AST.SelectionSet -> AST.SelectionSet
mergeSelectionSets (AST.SelectionSet selectionsA) (AST.SelectionSet selectionsB) =
    AST.SelectionSet (selectionsA ++ selectionsB)


join : SourceType nullability coreType -> SourceType nullability coreType -> SourceType nullability coreType
join a b =
    case ( a, b ) of
        ( SpecifiedType typeInfoA, SpecifiedType typeInfoB ) ->
            SpecifiedType
                { typeInfoA
                    | coreType = (typeInfoA.join typeInfoA.coreType typeInfoB.coreType)
                    , selectionSet = mergeSelectionSets typeInfoA.selectionSet typeInfoB.selectionSet
                }

        ( AnyType, _ ) ->
            b

        ( _, AnyType ) ->
            a


selectionSetFromSourceType : SourceType nullability coreType -> AST.SelectionSet
selectionSetFromSourceType sourceType =
    case sourceType of
        SpecifiedType { selectionSet } ->
            selectionSet

        AnyType ->
            emptySelectionSet


selectionSetFromSpec : Spec nullability coreType result -> AST.SelectionSet
selectionSetFromSpec (Spec sourceType _) =
    selectionSetFromSourceType sourceType


emptySelectionSet : AST.SelectionSet
emptySelectionSet =
    AST.SelectionSet []


primitiveSpec : coreType -> Decoder result -> Spec NonNull coreType result
primitiveSpec coreType decoder =
    Spec
        (SpecifiedType
            { nullability = nonNullFlag
            , coreType = coreType
            , join = always
            , selectionSet = emptySelectionSet
            }
        )
        (always decoder)


variableDefinitionAST :
    ( String, TypeRef.TypeRef, Maybe Value.Constant )
    -> AST.VariableDefinition
variableDefinitionAST ( name, typeRef, maybeDefaultValue ) =
    AST.VariableDefinition
        { name = name
        , variableType = typeRef
        , defaultValue = maybeDefaultValue
        }


directiveAST :
    ( String, List ( String, Value.Argument ) )
    -> AST.Directive
directiveAST ( name, arguments ) =
    AST.Directive
        { name = name
        , arguments = arguments
        }


operationTypeAST : OperationType operationType -> AST.OperationType
operationTypeAST operationType =
    case operationType of
        QueryOperationType ->
            AST.Query

        MutationOperationType ->
            AST.Mutation


operationAST : Operation operationType result -> AST.OperationDefinitionInfo
operationAST { operationType, name, variableDefinitions, directives, spec } =
    { operationType = operationTypeAST operationType
    , name = name
    , variableDefinitions = List.map variableDefinitionAST variableDefinitions
    , directives = List.map directiveAST directives
    , selectionSet = selectionSetFromSpec spec
    }


fragmentAST : Fragment result -> AST.FragmentDefinitionInfo
fragmentAST { name, typeCondition, directives, spec } =
    { name = name
    , typeCondition = typeCondition
    , directives = List.map directiveAST directives
    , selectionSet = selectionSetFromSpec spec
    }


requestAST : Request operationType result -> AST.Document
requestAST { operation } =
    AST.Document
        [ AST.OperationDefinition (operationAST operation) ]


responseDecoder :
    Request operationType result
    -> Decoder (Result (List Response.RequestError) result)
responseDecoder request =
    Response.decoder (specDecoder request.operation.spec)


specDecoder : Spec nullability coreType result -> Decoder result
specDecoder (Spec sourceType decoderFromSelectionSet) =
    sourceType
        |> selectionSetFromSourceType
        |> decoderFromSelectionSet
