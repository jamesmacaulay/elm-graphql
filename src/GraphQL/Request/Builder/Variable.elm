module GraphQL.Request.Builder.Variable
    exposing
        ( VariableSpec
        , Nullable
        , NonNull
        , Variable
        , Field
        , required
        , optional
        , int
        , float
        , string
        , bool
        , id
        , enum
        , nullable
        , list
        , object
        , field
        , optionalField
        , name
        , toDefinitionAST
        , extractValuesFrom
        )

{-| The functions in this module let you define GraphQL variables that you can pass as arguments in your request documents built with the functions in [`GraphQL.Request.Builder`](GraphQL-Request-Builder).

@docs VariableSpec, Nullable, NonNull, Variable, Field, required, optional, int, float, string, bool, id, enum, nullable, list, object, field, optionalField, name, toDefinitionAST, extractValuesFrom
-}

import GraphQL.Request.Document.AST as AST
import GraphQL.Request.Builder.TypeRef as TypeRef exposing (TypeRef)


{-| A specification for the type of a `Variable` which includes enough information to extract a conforming value from an Elm value.
-}
type VariableSpec nullability source
    = VariableSpec nullability TypeRef (source -> AST.ConstantValue)


{-| This type is used as a marker for the `VariableSpec` type's `nullability` parameter to indicate that the described GraphQL variable may be filled with a `null` value.
-}
type Nullable
    = Nullable


{-| This type is used as a marker for the `VariableSpec` type's `nullability` parameter to indicate that the described GraphQL variable will never be filled with a `null` value.
-}
type NonNull
    = NonNull


{-| This type represents a GraphQL variable definition, and includes enough information to extract a conforming GraphQL value from some arbitrary `source` type supplied by your Elm code.
-}
type Variable source
    = RequiredVariable String TypeRef (source -> AST.ConstantValue)
    | OptionalVariable String TypeRef (source -> Maybe AST.ConstantValue) AST.ConstantValue


{-| Describes a single field of a GraphQL Input Object type.
-}
type Field source
    = Field String TypeRef (source -> Maybe AST.ConstantValue)


{-| Construct a `Variable` that has no default value, and therefore must extract its value from a `source`. The first argument is the name of the variable that appears in the GraphQL request document, and must be unique for that document. It should _not_ include any leading dollar sign (`$`). The second argument is a function that extracts a value of the required type from a `source`. The third argument is a `VariableSpec` that describes the type of the variable.
-}
required : String -> (source -> a) -> VariableSpec nullability a -> Variable source
required name extract (VariableSpec _ typeRef convert) =
    RequiredVariable name typeRef (extract >> convert)


{-| Construct a `Variable` that has a default value, and therefore its `source` may or may not provide a value for it. The first three arguments are the same as for the `required` function, except that the function to extract a value from `source` must return a `Maybe` of the type expected by the `VariableSpec`. The last argument is a default value for the variable.

Note that the `VariableSpec` may be either `Nullable` or `NonNull`, but in both cases the variable definition is serialized _without_ a Non-Null modifier in the GraphQL request document, because optional variables may not be Non-Null in GraphQL. If you pass a `NonNull` `VariableSpec` into this function, it just means that you won't be able to represent an explicit `null` for the variable's value. If instead you pass a `Nullable` `VariableSpec` into this function, you will be able to represent an explicit `null` value for the variable, but you'll also have to deal with double-wrapped `Maybe` values – a missing value is then represented as a `Nothing` returned from your extraction function, and a `null` value is represented as `Just Nothing`. For this reason, it is recommended that you stick to `NonNull` `VariableSpec` values here unless you really need to be able to pass `null` explictly to the GraphQL server.
-}
optional : String -> (source -> Maybe a) -> VariableSpec nullability a -> a -> Variable source
optional name extractMaybe (VariableSpec nullability typeRef convert) defaultValue =
    OptionalVariable name (TypeRef.nullable typeRef) (extractMaybe >> Maybe.map convert) (convert defaultValue)


{-| A `VariableSpec` for the GraphQL `Int` type that extracts its value from an Elm `Int`.
-}
int : VariableSpec NonNull Int
int =
    VariableSpec NonNull TypeRef.int AST.IntValue


{-| A `VariableSpec` for the GraphQL `Float` type that extracts its value from an Elm `Float`.
-}
float : VariableSpec NonNull Float
float =
    VariableSpec NonNull TypeRef.float AST.FloatValue


{-| A `VariableSpec` for the GraphQL `String` type that extracts its value from an Elm `String`.
-}
string : VariableSpec NonNull String
string =
    VariableSpec NonNull TypeRef.string AST.StringValue


{-| A `VariableSpec` for the GraphQL `Boolean` type that extracts its value from an Elm `Bool`.
-}
bool : VariableSpec NonNull Bool
bool =
    VariableSpec NonNull TypeRef.boolean AST.BooleanValue


{-| A `VariableSpec` for the GraphQL `ID` type that extracts its value from an Elm `String`.
-}
id : VariableSpec NonNull String
id =
    VariableSpec NonNull TypeRef.id AST.StringValue


{-| Constructs a `VariableSpec` for a GraphQL Enum type. The first argument is the name of an Enum type as defined in the GraphQL schema being used. The second argument is a function that converts values of some arbitrary `source` type into `String` symbols that correspond to the Enum's possible values as defined in the schema.

    type AccessLevel
        = AdminAccess
        | MemberAccess

    accessLevelToEnumSymbol : AccessLevel -> String
    accessLevelToEnumSymbol accessLevel =
        case accessLevel of
            AdminAccess ->
                "ADMIN"

            MemberAccess ->
                "MEMBER"

    accessLevel : VariableSpec NonNull AccessLevel
    accessLevel =
        enum "AccessLevel" accessLevelToEnumSymbol
-}
enum : String -> (source -> String) -> VariableSpec NonNull source
enum typeName convert =
    VariableSpec NonNull (TypeRef.namedType typeName) (convert >> AST.EnumValue)


{-| Transforms a `NonNull` `VariableSpec` into one that allows `null` values, extracting its value from a `Maybe`.
-}
nullable : VariableSpec NonNull source -> VariableSpec Nullable (Maybe source)
nullable (VariableSpec NonNull typeRef convert) =
    VariableSpec
        Nullable
        (TypeRef.nullable typeRef)
        (Maybe.map convert >> Maybe.withDefault AST.NullValue)


{-| Constructs a `VariableSpec` for a GraphQL List type out of another `VariableSpec` that represents its items.
-}
list : VariableSpec nullability source -> VariableSpec NonNull (List source)
list (VariableSpec _ typeRef convert) =
    VariableSpec
        NonNull
        (TypeRef.list typeRef)
        (List.map convert >> AST.ListValue)


{-| Constructs a `VariableSpec` for a GraphQL Input Object type. The first argument is the name of an Input Object type as defined in the GraphQL schema being used. The second argument is a `List` of `Field` values constructed with the `field` function.

    type alias UpdateUserInputData =
        { name : String
        , email : String
        }

    userDataVar : Variable { vars | userData : UpdateUserInputData }
    userDataVar =
        required "userData"
            .userData
            (object "UpdateUserInput"
                [ field "name" .name string
                , field "email" .email string
                ]
            )
-}
object : String -> List (Field source) -> VariableSpec NonNull source
object typeName fields =
    VariableSpec
        NonNull
        (TypeRef.namedType typeName)
        (\source -> AST.ObjectValue (List.filterMap (fieldTuple source) fields))


{-| Constructs a `Field` to be passed to the `object` function. The first argument is the name of the field. The second argument is a function that extracts this field's value from the value that represents the whole object. See the documentation for `object` for more details.
-}
field :
    String
    -> (objVariableSource -> fieldVariableSource)
    -> VariableSpec nullability fieldVariableSource
    -> Field objVariableSource
field name extract (VariableSpec _ typeRef convert) =
    Field name typeRef (extract >> convert >> Just)


{-| Like `field`, except the extractor function must return a `Maybe` value. When the extracted value is `Nothing`, the field is not included in the object sent to the server at all. This works in a very similar way to the `optional` function, except that `optional` is used for entire optional variables while `optionalField` is used for optional fields of variable input objects.

The `VariableSpec` provided as the third argument may either be `Nullable` or `NonNull`. If the `VariableSpec` is `NonNull`, then the extracted value is wrapped in a single `Maybe` as described above. If it is `Nullable`, then a double-Maybe-wrapped value is extracted from the variables value. In this case, `Nothing` indicates that the field should be omitted, `Just Nothing` indicates an explicit `null` value, and `Just (Just x)` indicates a regular non-null value. In both cases, this function should never be used with fields of input objects that have been defined as non-null by the schema.

In the following example, both the `phoneNumber` and `email` fields are optional. However, only the `phoneNumber` field may be assigned an explicit `null` value (represented by `Just Nothing`):

    type alias UpdateUserInputData =
        { id : String
        , email : Maybe String
        , phoneNumber : Maybe (Maybe String)
        }

    userDataVar : Variable { vars | userData : UpdateUserInputData }
    userDataVar =
        required "userData"
            .userData
            (object "UpdateUserInput"
                [ field "id" .id id
                , optionalField "email" .email string
                , optionalField "phoneNumber" .phoneNumber (nullable string)
                ]
            )
-}
optionalField :
    String
    -> (objVariableSource -> Maybe fieldVariableSource)
    -> VariableSpec nullability fieldVariableSource
    -> Field objVariableSource
optionalField name extract (VariableSpec _ typeRef convert) =
    Field name typeRef (extract >> Maybe.map convert)


fieldTuple : source -> Field source -> Maybe ( String, AST.ConstantValue )
fieldTuple source (Field name _ convert) =
    convert source
        |> Maybe.map (\value -> ( name, value ))


valueFromSource : source -> Variable source -> Maybe ( String, AST.ConstantValue )
valueFromSource source var =
    case var of
        RequiredVariable _ _ f ->
            Just ( name var, (f source) )

        OptionalVariable _ _ f _ ->
            case f source of
                Nothing ->
                    Nothing

                Just value ->
                    Just ( name var, value )


{-| Returns the name of a `Variable` as it appears in a GraphQL request document, without any leading dollar sign (`$`).
-}
name : Variable source -> String
name var =
    case var of
        RequiredVariable name _ _ ->
            name

        OptionalVariable name _ _ _ ->
            name


{-| Returns the AST (abstract syntax tree) representation of a `Variable`.
-}
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


{-| Extracts generic values from a `source` and a `List` of zero or more compatible `Variable`s.
-}
extractValuesFrom : source -> List (Variable source) -> List ( String, AST.ConstantValue )
extractValuesFrom source vars =
    List.filterMap (valueFromSource source) vars
