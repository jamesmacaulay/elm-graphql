module GraphQL.Query.Encode exposing (..)

import GraphQL.Query exposing (..)
import GraphQL.Query.Arg as Arg
import String


encodeArgValue : Arg.Value -> String
encodeArgValue value =
    case value of
        Arg.VariableValue name ->
            "$" ++ name

        Arg.IntValue int ->
            toString int

        Arg.FloatValue float ->
            toString float

        Arg.StringValue string ->
            toString string

        Arg.BooleanValue True ->
            "true"

        Arg.BooleanValue False ->
            "false"

        Arg.NullValue ->
            "null"

        Arg.EnumValue symbol ->
            symbol

        Arg.ListValue values ->
            values |> List.map encodeArgValue |> toString

        Arg.ObjectValue pairs ->
            "{" ++ String.join ", " (List.map encodeArg pairs) ++ "}"


encodeArg : ( String, Arg.Value ) -> String
encodeArg ( name, value ) =
    name ++ ": " ++ encodeArgValue value


encodeArgList : List ( String, Arg.Value ) -> String
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
encodeField indentLevel (Field { name, spec, fieldAlias, args, directives }) =
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
            case getBaseSpec spec of
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
