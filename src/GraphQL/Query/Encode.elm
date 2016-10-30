module GraphQL.Query.Encode exposing (..)

import GraphQL.Query exposing (..)
import String


encodeArgValue : ArgValue -> String
encodeArgValue value =
    case value of
        VariableValue name ->
            "$" ++ name

        IntValue int ->
            toString int

        FloatValue float ->
            toString float

        StringValue string ->
            toString string

        BooleanValue True ->
            "true"

        BooleanValue False ->
            "false"

        NullValue ->
            "null"

        EnumValue symbol ->
            symbol

        ListValue values ->
            values |> List.map encodeArgValue |> toString

        ObjectValue pairs ->
            "{" ++ String.join ", " (List.map encodeArg pairs) ++ "}"


encodeArg : ( String, ArgValue ) -> String
encodeArg ( name, value ) =
    name ++ ": " ++ encodeArgValue value


encodeArgList : List ( String, ArgValue ) -> String
encodeArgList args =
    if List.isEmpty args then
        ""
    else
        "(" ++ String.join ", " (List.map encodeArg args) ++ ")"


encodeDirective : Directive -> String
encodeDirective (Directive { name, args }) =
    "@" ++ name ++ encodeArgList args


encodeDirectivesSuffix : List Directive -> String
encodeDirectivesSuffix =
    List.map (\dir -> " " ++ encodeDirective dir) >> String.join ""


indent : Int -> String -> String
indent level string =
    if level <= 0 then
        string
    else
        "  " ++ indent (level - 1) string


encodeSelectionSet : Int -> SelectionSet -> String
encodeSelectionSet indentLevel (SelectionSet selections) =
    "{\n"
        ++ String.join "\n" (List.map (encodeSelection (indentLevel + 1)) selections)
        ++ "\n"
        ++ indent indentLevel "}"


encodeField : Int -> Field -> String
encodeField indentLevel (Field { name, valueSpec, fieldAlias, args, directives }) =
    let
        aliasString =
            fieldAlias
                |> Maybe.map (\fieldAlias' -> fieldAlias' ++ ": ")
                |> Maybe.withDefault ""

        argsString =
            encodeArgList args

        directivesString =
            encodeDirectivesSuffix directives

        selectionSetString =
            case getBaseValueSpec valueSpec of
                ObjectSpec selectionSet ->
                    " " ++ encodeSelectionSet indentLevel selectionSet

                _ ->
                    ""
    in
        indent indentLevel (aliasString ++ name ++ argsString ++ directivesString ++ selectionSetString)


encodeSelection : Int -> Selection -> String
encodeSelection indentLevel selection =
    case selection of
        FieldSelection field ->
            encodeField indentLevel field

        FragmentSpreadSelection fragmentSpread ->
            encodeFragmentSpread indentLevel fragmentSpread

        InlineFragmentSelection inlineFragment ->
            encodeInlineFragment indentLevel inlineFragment


encodeFragmentSpread : Int -> FragmentSpread -> String
encodeFragmentSpread indentLevel (FragmentSpread { name, directives }) =
    let
        directivesString =
            encodeDirectivesSuffix directives
    in
        indent indentLevel ("... " ++ name ++ directivesString)


encodeInlineFragment : Int -> InlineFragment -> String
encodeInlineFragment indentLevel (InlineFragment { typeCondition, directives, selectionSet }) =
    let
        typeConditionString =
            typeCondition
                |> Maybe.map (\typeName -> " on " ++ typeName)
                |> Maybe.withDefault ""

        directivesString =
            encodeDirectivesSuffix directives

        selectionSetString =
            " " ++ encodeSelectionSet indentLevel selectionSet
    in
        indent indentLevel ("..." ++ typeConditionString ++ directivesString ++ selectionSetString)


encodeQuery : Query -> String
encodeQuery (Query { name, selectionSet }) =
    case name of
        Just name' ->
            "query " ++ name' ++ " " ++ encodeSelectionSet 0 selectionSet

        Nothing ->
            encodeSelectionSet 0 selectionSet
