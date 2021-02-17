module GraphQL.Request.Document.AST.Serialize exposing (serializeDocument)

import GraphQL.Request.Document.AST as AST
import Json.Encode as Encode
import String


serializeDocument : AST.Document -> String
serializeDocument (AST.Document definitions) =
    definitions
        |> List.map serializeDefinition
        |> String.join "\n\n"


listFromMaybe : Maybe a -> List a
listFromMaybe m =
    case m of
        Nothing ->
            []

        Just x ->
            [ x ]


serializeDefinition : AST.Definition -> String
serializeDefinition definition =
    case definition of
        AST.OperationDefinition operationInfo ->
            serializeOperation operationInfo

        AST.QueryShorthand selectionSet ->
            serializeSelectionSet 0 selectionSet |> String.join ""

        AST.FragmentDefinition fragmentInfo ->
            serializeFragmentDefinition fragmentInfo


serializeOperation : AST.OperationDefinitionInfo -> String
serializeOperation info =
    [ [ serializeOperationType info.operationType ]
    , listFromMaybe info.name
    , serializeVariableDefinitions info.variableDefinitions
    , List.map serializeDirective info.directives
    , serializeSelectionSet 0 info.selectionSet
    ]
        |> List.concat
        |> String.join " "


serializeOperationType : AST.OperationType -> String
serializeOperationType opType =
    case opType of
        AST.Query ->
            "query"

        AST.Mutation ->
            "mutation"


serializeVariableDefinitions : List AST.VariableDefinition -> List String
serializeVariableDefinitions defs =
    if List.isEmpty defs then
        []

    else
        [ "(" ++ String.join ", " (List.map serializeVariableDefinition defs) ++ ")" ]


serializeVariableDefinition : AST.VariableDefinition -> String
serializeVariableDefinition (AST.VariableDefinition info) =
    [ [ serializeVariableName info.name ++ ":", serializeTypeRef info.variableType ]
    , listFromMaybe (Maybe.map serializeDefaultValue info.defaultValue)
    ]
        |> List.concat
        |> String.join " "


serializeVariableName : String -> String
serializeVariableName name =
    "$" ++ name


serializeTypeRef : AST.TypeRef -> String
serializeTypeRef (AST.TypeRef nullability coreTypeRef) =
    serializeCoreTypeRef coreTypeRef ++ serializeNullability nullability


serializeDefaultValue : AST.ConstantValue -> String
serializeDefaultValue value =
    "= " ++ serializeValue value


serializeNullability : AST.Nullability -> String
serializeNullability nullability =
    case nullability of
        AST.Nullable ->
            ""

        AST.NonNull ->
            "!"


serializeCoreTypeRef : AST.CoreTypeRef -> String
serializeCoreTypeRef coreTypeRef =
    case coreTypeRef of
        AST.NamedTypeRef name ->
            name

        AST.ListTypeRef typeRef ->
            "[" ++ serializeTypeRef typeRef ++ "]"


serializeValue : AST.Value variableConstraint -> String
serializeValue value =
    case value of
        AST.VariableValue _ name ->
            "$" ++ name

        AST.IntValue int ->
            String.fromInt int

        AST.FloatValue float ->
            String.fromFloat float

        AST.StringValue string ->
            Encode.encode 0 (Encode.string string)

        AST.BooleanValue True ->
            "true"

        AST.BooleanValue False ->
            "false"

        AST.NullValue ->
            "null"

        AST.EnumValue symbol ->
            symbol

        AST.ListValue values ->
            "[" ++ String.join ", " (List.map serializeValue values) ++ "]"

        AST.ObjectValue pairs ->
            "{" ++ String.join ", " (List.map serializeKeyValuePair pairs) ++ "}"


serializeKeyValuePair : ( String, AST.Value variableConstraint ) -> String
serializeKeyValuePair ( key, value ) =
    key ++ ": " ++ serializeValue value


serializeDirective : AST.Directive -> String
serializeDirective (AST.Directive { name, arguments }) =
    String.join "" (serializeDirectiveName name :: serializeArgList arguments)


serializeDirectiveName : String -> String
serializeDirectiveName name =
    "@" ++ name


serializeArgList : List ( String, AST.ArgumentValue ) -> List String
serializeArgList args =
    if List.isEmpty args then
        []

    else
        [ "(" ++ String.join ", " (List.map serializeKeyValuePair args) ++ ")" ]


serializeSelectionSet : Int -> AST.SelectionSet -> List String
serializeSelectionSet indentLevel (AST.SelectionSet selections) =
    if List.isEmpty selections then
        []

    else
        [ "{\n"
            ++ String.join "\n" (List.map (serializeSelection (indentLevel + 1)) selections)
            ++ "\n"
            ++ indent indentLevel "}"
        ]


indent : Int -> String -> String
indent level string =
    if level <= 0 then
        string

    else
        "  " ++ indent (level - 1) string


serializeSelection : Int -> AST.Selection -> String
serializeSelection indentLevel selection =
    case selection of
        AST.Field field ->
            serializeField indentLevel field

        AST.FragmentSpread fragmentSpread ->
            serializeFragmentSpread indentLevel fragmentSpread

        AST.InlineFragment inlineFragment ->
            serializeInlineFragment indentLevel inlineFragment


serializeField : Int -> AST.FieldInfo -> String
serializeField indentLevel field =
    [ listFromMaybe (Maybe.map serializeFieldAlias field.alias)
    , [ String.join "" (field.name :: serializeArgList field.arguments) ]
    , List.map serializeDirective field.directives
    , serializeSelectionSet indentLevel field.selectionSet
    ]
        |> List.concat
        |> String.join " "
        |> indent indentLevel


serializeFieldAlias : String -> String
serializeFieldAlias alias =
    alias ++ ":"


serializeFragmentSpread : Int -> AST.FragmentSpreadInfo -> String
serializeFragmentSpread indentLevel { name, directives } =
    (serializeFragmentSpreadName name :: List.map serializeDirective directives)
        |> String.join " "
        |> indent indentLevel


serializeFragmentSpreadName : String -> String
serializeFragmentSpreadName name =
    "..." ++ name


serializeInlineFragment : Int -> AST.InlineFragmentInfo -> String
serializeInlineFragment indentLevel { typeCondition, directives, selectionSet } =
    [ "..." :: listFromMaybe (Maybe.map serializeTypeCondition typeCondition)
    , List.map serializeDirective directives
    , serializeSelectionSet indentLevel selectionSet
    ]
        |> List.concat
        |> String.join " "
        |> indent indentLevel


serializeTypeCondition : AST.TypeCondition -> String
serializeTypeCondition (AST.TypeCondition namedType) =
    "on " ++ namedType


serializeFragmentDefinition : AST.FragmentDefinitionInfo -> String
serializeFragmentDefinition { name, typeCondition, directives, selectionSet } =
    [ [ "fragment", name, serializeTypeCondition typeCondition ]
    , List.map serializeDirective directives
    , serializeSelectionSet 0 selectionSet
    ]
        |> List.concat
        |> String.join " "
