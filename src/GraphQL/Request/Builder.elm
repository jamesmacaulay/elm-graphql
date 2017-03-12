module GraphQL.Request.Builder
    exposing
        ( Request
        , Document
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
        , requestBody
        , requestBodyAST
        , requestVariableValues
        , responseDecoder
        , queryDocument
        , mutationDocument
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
        )

import Json.Decode as Decode exposing (Decoder)
import GraphQL.Request.Document.AST as AST
import GraphQL.Request.Document.AST.Serialize as Serialize
import GraphQL.Request.Document.AST.Util as Util
import GraphQL.Request.Builder.TypeRef as TypeRef
import GraphQL.Request.Builder.Value as Value
import GraphQL.Request.Builder.Variable as Variable exposing (Variable)
import GraphQL.Response as Response
import Dict exposing (Dict)
import Set exposing (Set)


type Spec nullability coreType variableSource result
    = Spec (SourceType nullability coreType) (AST.SelectionSet -> Decoder result) (List (Variable variableSource))


type alias TypeCondition =
    AST.TypeCondition


type Fragment variableSource result
    = Fragment
        { name : String
        , typeCondition : TypeCondition
        , directives : List ( String, List ( String, Value.Argument variableSource ) )
        , spec : Spec NonNull ObjectType variableSource result
        }


type Request operationType variableSource result
    = Request
        { document : Document operationType variableSource result
        , variableSource : variableSource
        , variableValues : List ( String, AST.ConstantValue )
        }


type Document operationType variableSource result
    = Document
        { operation : Operation operationType variableSource result
        , ast : AST.Document
        , serialized : String
        }


type Operation operationType variableSource result
    = Operation
        { operationType : OperationType operationType
        , name : Maybe String
        , directives : List ( String, List ( String, Value.Argument variableSource ) )
        , spec : Spec NonNull ObjectType variableSource result
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


type FieldOption variableSource
    = FieldAlias String
    | FieldArgs (List ( String, Value.Argument variableSource ))
    | FieldDirective String (List ( String, Value.Argument variableSource ))


request :
    variableSource
    -> Document operationType variableSource result
    -> Request operationType variableSource result
request variableSource document =
    Request
        { document = document
        , variableSource = variableSource
        , variableValues = (documentVariables document |> Variable.extractValuesFrom variableSource)
        }


requestBody : Request operationType variableSource result -> String
requestBody (Request { document }) =
    documentString document


requestBodyAST : Request operationType variableSource result -> AST.Document
requestBodyAST (Request { document }) =
    documentAST document


requestVariableValues : Request operationType variableSource result -> List ( String, AST.ConstantValue )
requestVariableValues (Request { variableValues }) =
    variableValues


responseDecoder : Request operationType variableSource result -> Decoder result
responseDecoder (Request { document }) =
    documentResponseDecoder document
        |> Response.successDecoder


document : Operation operationType variableSource result -> Document operationType variableSource result
document operation =
    let
        ast =
            AST.Document
                [ AST.OperationDefinition (operationAST operation) ]
    in
        Document
            { operation = operation
            , ast = ast
            , serialized = Serialize.serializeDocument ast
            }


queryDocument :
    Spec NonNull ObjectType variableSource result
    -> Document Query variableSource result
queryDocument spec =
    document
        (Operation
            { operationType = queryOperationType
            , name = Nothing
            , directives = []
            , spec = spec
            }
        )


queryOperationType : OperationType Query
queryOperationType =
    QueryOperationType


mutationDocument :
    Spec NonNull ObjectType variableSource result
    -> Document Mutation variableSource result
mutationDocument spec =
    document
        (Operation
            { operationType = mutationOperationType
            , name = Nothing
            , directives = []
            , spec = spec
            }
        )


mutationOperationType : OperationType Mutation
mutationOperationType =
    MutationOperationType


fragment :
    String
    -> TypeCondition
    -> Spec NonNull ObjectType variableSource result
    -> Fragment variableSource result
fragment name typeCondition spec =
    Fragment
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
    -> Spec NonNull ObjectType variableSource (fieldValue -> result)
object ctr =
    Spec emptyObjectSpecifiedType (always (Decode.succeed ctr)) []


withField :
    String
    -> List (FieldOption variableSource)
    -> Spec nullability coreType variableSource a
    -> Spec NonNull ObjectType variableSource (a -> b)
    -> Spec NonNull ObjectType variableSource b
withField name fieldOptions spec fSpec =
    fSpec
        |> andMap (field name fieldOptions spec)


field :
    String
    -> List (FieldOption variableSource)
    -> Spec nullability coreType variableSource result
    -> Spec NonNull ObjectType variableSource result
field name fieldOptions (Spec sourceType fieldDecoder fieldVars) =
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

        vars =
            mergeVariables (List.concatMap varsFromFieldOption fieldOptions) fieldVars
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
            vars


alias : String -> FieldOption variableSource
alias =
    FieldAlias


args : List ( String, Value.Argument variableSource ) -> FieldOption variableSource
args =
    FieldArgs


directive : String -> List ( String, Value.Argument variableSource ) -> FieldOption variableSource
directive =
    FieldDirective


varsFromFieldOption : FieldOption variableSource -> List (Variable variableSource)
varsFromFieldOption fieldOption =
    case fieldOption of
        FieldAlias _ ->
            []

        FieldArgs arguments ->
            List.concatMap (Value.getVariables << Tuple.second) arguments

        FieldDirective _ arguments ->
            List.concatMap (Value.getVariables << Tuple.second) arguments


int : Spec NonNull IntType variableSource Int
int =
    primitiveSpec IntType Decode.int


float : Spec NonNull FloatType variableSource Float
float =
    primitiveSpec FloatType Decode.float


string : Spec NonNull StringType variableSource String
string =
    primitiveSpec StringType Decode.string


bool : Spec NonNull BooleanType variableSource Bool
bool =
    primitiveSpec BooleanType Decode.bool


id : Spec NonNull IdType variableSource String
id =
    primitiveSpec IdType Decode.string


enum : List ( String, a ) -> Spec NonNull EnumType variableSource a
enum =
    enumWithFallback
        (\label ->
            Decode.fail ("Unexpected enum value " ++ toString label)
        )


enumWithDefault : (String -> a) -> List ( String, a ) -> Spec NonNull EnumType variableSource a
enumWithDefault ctr =
    enumWithFallback
        (\label ->
            Decode.succeed (ctr label)
        )


enumWithFallback : (String -> Decoder a) -> List ( String, a ) -> Spec NonNull EnumType variableSource a
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
            []


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
    Spec itemNullability itemType variableSource result
    -> Spec NonNull (ListType itemNullability itemType) variableSource (List result)
list (Spec itemType decoder vars) =
    Spec
        (SpecifiedType
            { nullability = nonNullFlag
            , coreType = (ListType itemType)
            , join = listJoin
            , selectionSet = selectionSetFromSourceType itemType
            }
        )
        (Decode.list << decoder)
        vars


nullable : Spec NonNull coreType variableSource result -> Spec Nullable coreType variableSource (Maybe result)
nullable (Spec sourceType decoder vars) =
    case sourceType of
        SpecifiedType typeInfo ->
            Spec
                (SpecifiedType { typeInfo | nullability = nullableFlag })
                (Decode.nullable << decoder)
                vars

        AnyType ->
            Spec AnyType (Decode.nullable << decoder) vars


emptyObjectSpecifiedType : SourceType NonNull ObjectType
emptyObjectSpecifiedType =
    SpecifiedType
        { nullability = nonNullFlag
        , coreType = ObjectType
        , join = always
        , selectionSet = emptySelectionSet
        }


produce : result -> Spec nullability coreType variableSource result
produce x =
    Spec AnyType (always (Decode.succeed x)) []


map : (a -> b) -> Spec nullability coreType variableSource a -> Spec nullability coreType variableSource b
map f (Spec sourceType decoder vars) =
    Spec sourceType (decoder >> Decode.map f) vars


map2 :
    (a -> b -> c)
    -> Spec nullability coreType variableSource a
    -> Spec nullability coreType variableSource b
    -> Spec nullability coreType variableSource c
map2 f (Spec sourceTypeA decoderA varsA) (Spec sourceTypeB decoderB varsB) =
    let
        joinedSourceType =
            join sourceTypeA sourceTypeB

        joinedDecoder selectionSet =
            Decode.map2 f (decoderA selectionSet) (decoderB selectionSet)

        mergedVariables =
            mergeVariables varsA varsB
    in
        Spec joinedSourceType joinedDecoder mergedVariables


map3 :
    (a -> b -> c -> d)
    -> Spec nullability coreType variableSource a
    -> Spec nullability coreType variableSource b
    -> Spec nullability coreType variableSource c
    -> Spec nullability coreType variableSource d
map3 f s1 s2 s3 =
    map f s1
        |> andMap s2
        |> andMap s3


map4 :
    (a -> b -> c -> d -> e)
    -> Spec nullability coreType variableSource a
    -> Spec nullability coreType variableSource b
    -> Spec nullability coreType variableSource c
    -> Spec nullability coreType variableSource d
    -> Spec nullability coreType variableSource e
map4 f s1 s2 s3 s4 =
    map f s1
        |> andMap s2
        |> andMap s3
        |> andMap s4


map5 :
    (a -> b -> c -> d -> e -> f)
    -> Spec nullability coreType variableSource a
    -> Spec nullability coreType variableSource b
    -> Spec nullability coreType variableSource c
    -> Spec nullability coreType variableSource d
    -> Spec nullability coreType variableSource e
    -> Spec nullability coreType variableSource f
map5 f s1 s2 s3 s4 s5 =
    map f s1
        |> andMap s2
        |> andMap s3
        |> andMap s4
        |> andMap s5


map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Spec nullability coreType variableSource a
    -> Spec nullability coreType variableSource b
    -> Spec nullability coreType variableSource c
    -> Spec nullability coreType variableSource d
    -> Spec nullability coreType variableSource e
    -> Spec nullability coreType variableSource f
    -> Spec nullability coreType variableSource g
map6 f s1 s2 s3 s4 s5 s6 =
    map f s1
        |> andMap s2
        |> andMap s3
        |> andMap s4
        |> andMap s5
        |> andMap s6


map7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> Spec nullability coreType variableSource a
    -> Spec nullability coreType variableSource b
    -> Spec nullability coreType variableSource c
    -> Spec nullability coreType variableSource d
    -> Spec nullability coreType variableSource e
    -> Spec nullability coreType variableSource f
    -> Spec nullability coreType variableSource g
    -> Spec nullability coreType variableSource h
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
    -> Spec nullability coreType variableSource a
    -> Spec nullability coreType variableSource b
    -> Spec nullability coreType variableSource c
    -> Spec nullability coreType variableSource d
    -> Spec nullability coreType variableSource e
    -> Spec nullability coreType variableSource f
    -> Spec nullability coreType variableSource g
    -> Spec nullability coreType variableSource h
    -> Spec nullability coreType variableSource i
map8 f s1 s2 s3 s4 s5 s6 s7 s8 =
    map f s1
        |> andMap s2
        |> andMap s3
        |> andMap s4
        |> andMap s5
        |> andMap s6
        |> andMap s7
        |> andMap s8


andMap : Spec nullability coreType variableSource a -> Spec nullability coreType variableSource (a -> b) -> Spec nullability coreType variableSource b
andMap specA specF =
    map2 (<|) specF specA


applyFieldOption : FieldOption variableSource -> AST.FieldInfo -> AST.FieldInfo
applyFieldOption fieldOption field =
    case fieldOption of
        FieldAlias name ->
            { field | alias = Just name }

        FieldArgs arguments ->
            { field
                | arguments =
                    field.arguments
                        ++ List.map (Tuple.mapSecond Value.getAST) arguments
            }

        FieldDirective name arguments ->
            { field
                | directives =
                    field.directives
                        ++ [ AST.Directive
                                { name = name
                                , arguments = List.map (Tuple.mapSecond Value.getAST) arguments
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


selectionSetFromSpec : Spec nullability coreType variableSource result -> AST.SelectionSet
selectionSetFromSpec (Spec sourceType _ _) =
    selectionSetFromSourceType sourceType


emptySelectionSet : AST.SelectionSet
emptySelectionSet =
    AST.SelectionSet []


primitiveSpec : coreType -> Decoder result -> Spec NonNull coreType variableSource result
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
        []


variableDefinitionsAST :
    Spec nullability coreType variableSource result
    -> List AST.VariableDefinition
variableDefinitionsAST (Spec _ _ vars) =
    List.map Variable.toDefinitionAST vars


directiveAST :
    ( String, List ( String, Value.Argument a ) )
    -> AST.Directive
directiveAST ( name, arguments ) =
    AST.Directive
        { name = name
        , arguments = List.map (Tuple.mapSecond Value.getAST) arguments
        }


operationTypeAST : OperationType operationType -> AST.OperationType
operationTypeAST operationType =
    case operationType of
        QueryOperationType ->
            AST.Query

        MutationOperationType ->
            AST.Mutation


operationAST : Operation operationType variableSource result -> AST.OperationDefinitionInfo
operationAST (Operation { operationType, name, directives, spec }) =
    { operationType = operationTypeAST operationType
    , name = name
    , variableDefinitions = variableDefinitionsAST spec
    , directives = List.map directiveAST directives
    , selectionSet = selectionSetFromSpec spec
    }


fragmentAST : Fragment variableSource result -> AST.FragmentDefinitionInfo
fragmentAST (Fragment { name, typeCondition, directives, spec }) =
    { name = name
    , typeCondition = typeCondition
    , directives = List.map directiveAST directives
    , selectionSet = selectionSetFromSpec spec
    }


documentAST : Document operationType variableSource result -> AST.Document
documentAST (Document { ast }) =
    ast


documentString : Document operationType variableSource result -> String
documentString (Document { serialized }) =
    serialized


documentResponseDecoder :
    Document operationType variableSource result
    -> Decoder result
documentResponseDecoder (Document { operation }) =
    let
        (Operation { spec }) =
            operation
    in
        specDecoder spec


documentVariables : Document operationType variableSource result -> List (Variable variableSource)
documentVariables (Document { operation }) =
    let
        (Operation { spec }) =
            operation

        (Spec _ _ vars) =
            spec
    in
        vars


specDecoder : Spec nullability coreType variableSource result -> Decoder result
specDecoder (Spec sourceType decoderFromSelectionSet _) =
    sourceType
        |> selectionSetFromSourceType
        |> decoderFromSelectionSet


mergeVariables : List (Variable source) -> List (Variable source) -> List (Variable source)
mergeVariables varsA varsB =
    varsA ++ varsB
