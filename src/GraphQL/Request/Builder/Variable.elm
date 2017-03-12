module GraphQL.Request.Builder.Variable exposing (..)

import GraphQL.Request.Document.AST as AST
import GraphQL.Request.Builder.TypeRef as TypeRef exposing (TypeRef)


type Spec nullability source
    = Spec nullability TypeRef (source -> AST.ConstantValue)


type Nullable
    = Nullable


type NonNull
    = NonNull


type Variable source
    = RequiredVariable String TypeRef (source -> AST.ConstantValue)
    | OptionalVariable String TypeRef (source -> Maybe AST.ConstantValue) AST.ConstantValue


type Field source
    = Field String TypeRef (source -> AST.ConstantValue)


name : Variable a -> String
name var =
    case var of
        RequiredVariable name _ _ ->
            name

        OptionalVariable name _ _ _ ->
            name


required : String -> (a -> b) -> Spec nullability b -> Variable a
required name extract (Spec _ typeRef convert) =
    RequiredVariable name typeRef (extract >> convert)


optional : String -> (a -> Maybe b) -> Spec Nullable b -> b -> Variable a
optional name extractMaybe (Spec Nullable typeRef convert) defaultValue =
    OptionalVariable name typeRef (extractMaybe >> Maybe.map convert) (convert defaultValue)


int : Spec NonNull Int
int =
    Spec NonNull TypeRef.int AST.IntValue


float : Spec NonNull Float
float =
    Spec NonNull TypeRef.float AST.FloatValue


string : Spec NonNull String
string =
    Spec NonNull TypeRef.string AST.StringValue


bool : Spec NonNull Bool
bool =
    Spec NonNull TypeRef.boolean AST.BooleanValue


id : Spec NonNull String
id =
    Spec NonNull TypeRef.id AST.StringValue


nullable : Spec NonNull source -> Spec Nullable (Maybe source)
nullable (Spec NonNull typeRef convert) =
    Spec
        Nullable
        (TypeRef.nullable typeRef)
        (Maybe.map convert >> Maybe.withDefault AST.NullValue)


list : Spec nullability source -> Spec NonNull (List source)
list (Spec _ typeRef convert) =
    Spec
        NonNull
        (TypeRef.list typeRef)
        (List.map convert >> AST.ListValue)


fieldTuple : source -> Field source -> ( String, AST.ConstantValue )
fieldTuple source (Field name _ convert) =
    ( name, convert source )


object : String -> List (Field source) -> Spec NonNull source
object typeName fields =
    Spec
        NonNull
        (TypeRef.namedType typeName)
        (\source -> AST.ObjectValue (List.map (fieldTuple source) fields))


field : String -> (obj -> field) -> Spec nullability field -> Field obj
field name extract (Spec _ typeRef convert) =
    Field name typeRef (extract >> convert)


toDefinitionAST : Variable source -> AST.VariableDefinition
toDefinitionAST var =
    case var of
        RequiredVariable name typeRef _ ->
            AST.VariableDefinition
                { name = name
                , variableType = typeRef
                , defaultValue = Nothing
                }

        OptionalVariable name typeRef _ defaultValue ->
            AST.VariableDefinition
                { name = name
                , variableType = typeRef
                , defaultValue = Just defaultValue
                }


valueFromSource : source -> Variable source -> Maybe ( String, AST.ConstantValue )
valueFromSource source var =
    case var of
        RequiredVariable _ _ f ->
            Just ( name var, (f source) )

        OptionalVariable _ _ f _ ->
            f source
                |> Maybe.map ((,) (name var))


extractValuesFrom : source -> List (Variable source) -> List ( String, AST.ConstantValue )
extractValuesFrom source vars =
    List.filterMap (valueFromSource source) vars
