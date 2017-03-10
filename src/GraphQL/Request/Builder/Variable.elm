module Variable exposing (..)

import GraphQL.Request.Builder.TypeRef as TypeRef exposing (TypeRef)
import GraphQL.Request.Builder.Value as Value


type Spec nullability source
    = Spec nullability TypeRef (source -> Value.Constant)


type Nullable
    = Nullable


type NonNull
    = NonNull


type Variable source
    = RequiredVariable String TypeRef (source -> Value.Constant)
    | OptionalVariable String TypeRef (source -> Maybe Value.Constant) Value.Constant


type Field source
    = Field String TypeRef (source -> Value.Constant)


required : String -> (a -> b) -> Spec nullability b -> Variable a
required name extract (Spec _ typeRef convert) =
    RequiredVariable name typeRef (extract >> convert)


optional : String -> (a -> Maybe b) -> Value.Constant -> Spec Nullable b -> Variable a
optional name extractMaybe defaultValue (Spec Nullable typeRef convert) =
    OptionalVariable name typeRef (extractMaybe >> Maybe.map convert) defaultValue


int : Spec NonNull Int
int =
    Spec NonNull TypeRef.int Value.int


float : Spec NonNull Float
float =
    Spec NonNull TypeRef.float Value.float


string : Spec NonNull String
string =
    Spec NonNull TypeRef.string Value.string


bool : Spec NonNull Bool
bool =
    Spec NonNull TypeRef.boolean Value.bool


id : Spec NonNull String
id =
    Spec NonNull TypeRef.id Value.string


nullable : Spec NonNull source -> Spec Nullable (Maybe source)
nullable (Spec NonNull typeRef convert) =
    Spec
        Nullable
        (TypeRef.nullable typeRef)
        (Maybe.map convert >> Maybe.withDefault Value.null)


list : Spec nullability source -> Spec NonNull (List source)
list (Spec _ typeRef convert) =
    Spec
        NonNull
        (TypeRef.list typeRef)
        (List.map convert >> Value.list)


fieldTuple : source -> Field source -> ( String, Value.Constant )
fieldTuple source (Field name _ convert) =
    ( name, convert source )


object : String -> List (Field source) -> Spec NonNull source
object typeName fields =
    Spec
        NonNull
        (TypeRef.namedType typeName)
        (\source -> Value.object (List.map (fieldTuple source) fields))


field : String -> (obj -> field) -> Spec nullability field -> Field obj
field name extract (Spec _ typeRef convert) =
    Field name typeRef (extract >> convert)
