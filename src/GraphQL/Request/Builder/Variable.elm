module GraphQL.Request.Builder.Variable
    exposing
        ( Spec
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
        , nullable
        , list
        , object
        , field
        , name
        , toDefinitionAST
        , extractValuesFrom
        )

{-| The functions in this module let you define GraphQL variables that you can pass as arguments in your request documents built with the functions in [`GraphQL.Request.Builder`](GraphQL-Request-Builder).

@docs Spec, Nullable, NonNull, Variable, Field, required, optional, int, float, string, bool, id, nullable, list, object, field, name, toDefinitionAST, extractValuesFrom
-}

import GraphQL.Request.Document.AST as AST
import GraphQL.Request.Builder.TypeRef as TypeRef exposing (TypeRef)


{-| A specification for the type of a `Variable` which includes enough information to extract a conforming value from an Elm value.
-}
type Spec nullability source
    = Spec nullability TypeRef (source -> AST.ConstantValue)


{-| This type is used as a marker for the `Spec` type's `nullability` parameter to indicate that the described GraphQL variable may be filled with a `null` value.
-}
type Nullable
    = Nullable


{-| This type is used as a marker for the `Spec` type's `nullability` parameter to indicate that the described GraphQL variable will never be filled with a `null` value.
-}
type NonNull
    = NonNull


{-| This type represents a GraphQL variable definition, and includes enough information to extract a conforming GraphQL value from some arbitrary `variableSource` type supplied by your Elm code.
-}
type Variable variableSource
    = RequiredVariable String TypeRef (variableSource -> AST.ConstantValue)
    | OptionalVariable String TypeRef (variableSource -> Maybe AST.ConstantValue) AST.ConstantValue


{-| Describes a single field of a GraphQL Input Object type.
-}
type Field variableSource
    = Field String TypeRef (variableSource -> AST.ConstantValue)


{-| Construct a `Variable` that has no default value, and therefore must extract its value from a `variableSource`. The first argument is the name of the variable that appears in the GraphQL request document, and must be unique for that document. It should _not_ include any leading dollar sign (`$`). The second argument is a function that extracts a value of the required type from a `variableSource`. The third argument is a `Spec` that describes the type of the variable.
-}
required : String -> (variableSource -> a) -> Spec nullability a -> Variable variableSource
required name extract (Spec _ typeRef convert) =
    RequiredVariable name typeRef (extract >> convert)


{-| Construct a `Variable` that has a default value, and therefore its `variableSource` may or may not provide a value for it. The first three arguments are the same as for the `required` function, except that the function to extract a value from `variableSource` must return a `Maybe` of the type expected by the `Spec`. The last argument is a default value for the variable.

Note that the `Spec` may be either `Nullable` or `NonNull`, but in both cases the variable definition is serialized _without_ a Non-Null modifier in the GraphQL request document, because optional variables may not be Non-Null in GraphQL. If you pass a `NonNull` `Spec` into this function, it just means that you won't be able to represent an explicit `null` for the variable's value. If instead you pass a `Nullable` `Spec` into this function, you will be able to represent an explicit `null` value for the variable, but you'll also have to deal with double-wrapped `Maybe` values – a missing value is then represented as a `Nothing` returned from your extraction function, and a `null` value is represented as `Just Nothing`. For this reason, it is recommended that you stick to `NonNull` `Spec` values here unless you really need to be able to pass `null` explictly to the GraphQL server.
-}
optional : String -> (variableSource -> Maybe a) -> Spec nullability a -> a -> Variable variableSource
optional name extractMaybe (Spec nullability typeRef convert) defaultValue =
    OptionalVariable name (TypeRef.nullable typeRef) (extractMaybe >> Maybe.map convert) (convert defaultValue)


{-| A `Spec` for the GraphQL `Int` type that extracts its value from an Elm `Int`.
-}
int : Spec NonNull Int
int =
    Spec NonNull TypeRef.int AST.IntValue


{-| A `Spec` for the GraphQL `Float` type that extracts its value from an Elm `Float`.
-}
float : Spec NonNull Float
float =
    Spec NonNull TypeRef.float AST.FloatValue


{-| A `Spec` for the GraphQL `String` type that extracts its value from an Elm `String`.
-}
string : Spec NonNull String
string =
    Spec NonNull TypeRef.string AST.StringValue


{-| A `Spec` for the GraphQL `Boolean` type that extracts its value from an Elm `Bool`.
-}
bool : Spec NonNull Bool
bool =
    Spec NonNull TypeRef.boolean AST.BooleanValue


{-| A `Spec` for the GraphQL `ID` type that extracts its value from an Elm `String`.
-}
id : Spec NonNull String
id =
    Spec NonNull TypeRef.id AST.StringValue


{-| Transforms a `NonNull` `Spec` into one that allows `null` values, extracting its value from a `Maybe`.
-}
nullable : Spec NonNull variableSource -> Spec Nullable (Maybe variableSource)
nullable (Spec NonNull typeRef convert) =
    Spec
        Nullable
        (TypeRef.nullable typeRef)
        (Maybe.map convert >> Maybe.withDefault AST.NullValue)


{-| Constructs a `Spec` for a GraphQL List type out of another `Spec` that represents its items.
-}
list : Spec nullability variableSource -> Spec NonNull (List variableSource)
list (Spec _ typeRef convert) =
    Spec
        NonNull
        (TypeRef.list typeRef)
        (List.map convert >> AST.ListValue)


{-| Constructs a `Spec` for a GraphQL Input Object type. The first argument is the name of an Input Object type as defined in the GraphQL schema being used. The second argument is a `List` of `Field` values constructed with the `field` function.

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
object : String -> List (Field variableSource) -> Spec NonNull variableSource
object typeName fields =
    Spec
        NonNull
        (TypeRef.namedType typeName)
        (\source -> AST.ObjectValue (List.map (fieldTuple source) fields))


{-| Constructs a `Field` to be passed to the `object` function. The first argument is the name of the field. The second argument is a function that extracts this field's value from the value that represents the whole object. See the documentation for `object` for more details.
-}
field :
    String
    -> (objVariableSource -> fieldVariableSource)
    -> Spec nullability fieldVariableSource
    -> Field objVariableSource
field name extract (Spec _ typeRef convert) =
    Field name typeRef (extract >> convert)


fieldTuple : variableSource -> Field variableSource -> ( String, AST.ConstantValue )
fieldTuple source (Field name _ convert) =
    ( name, convert source )


valueFromSource : variableSource -> Variable variableSource -> Maybe ( String, AST.ConstantValue )
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
name : Variable variableSource -> String
name var =
    case var of
        RequiredVariable name _ _ ->
            name

        OptionalVariable name _ _ _ ->
            name


{-| Returns the AST (abstract syntax tree) representation of a `Variable`.
-}
toDefinitionAST : Variable variableSource -> AST.VariableDefinition
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


{-| Extracts generic values from a `variableSource` and a `List` of zero or more compatible `Variable`s.
-}
extractValuesFrom : variableSource -> List (Variable variableSource) -> List ( String, AST.ConstantValue )
extractValuesFrom source vars =
    List.filterMap (valueFromSource source) vars
