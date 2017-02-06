module GraphQL.Query.Builder.Encode exposing (..)

import GraphQL.Query.Builder.Structure exposing (..)
import GraphQL.Query.Builder.Arg as Arg
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
encodeDirective { name, args } =
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


encodeSelections : Int -> List Selection -> String
encodeSelections indentLevel selections =
    "{\n"
        ++ String.join "\n" (List.map (encodeSelection (indentLevel + 1)) selections)
        ++ "\n"
        ++ indent indentLevel "}"


encodeSelectionSet : Int -> Spec -> String
encodeSelectionSet indentLevel spec =
    case getBaseSpec spec of
        ObjectSpec selections ->
            encodeSelections indentLevel selections

        _ ->
            ""


encodeSelectionSetSuffix : Int -> Spec -> String
encodeSelectionSetSuffix indentLevel spec =
    case getBaseSpec spec of
        ObjectSpec selections ->
            " " ++ encodeSelections indentLevel selections

        _ ->
            ""


encodeField : Int -> Field -> String
encodeField indentLevel { name, spec, fieldAlias, args, directives } =
    let
        aliasString =
            fieldAlias
                |> Maybe.map (\x -> x ++ ": ")
                |> Maybe.withDefault ""

        argsString =
            encodeArgList args

        directivesString =
            encodeDirectivesSuffix directives

        selectionSetString =
            encodeSelectionSetSuffix indentLevel spec
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
encodeFragmentSpread indentLevel { name, directives } =
    let
        directivesString =
            encodeDirectivesSuffix directives
    in
        indent indentLevel ("... " ++ name ++ directivesString)


encodeInlineFragment : Int -> InlineFragment -> String
encodeInlineFragment indentLevel { typeCondition, directives, spec } =
    let
        typeConditionString =
            typeCondition
                |> Maybe.map (\typeName -> " on " ++ typeName)
                |> Maybe.withDefault ""

        directivesString =
            encodeDirectivesSuffix directives

        selectionSetString =
            encodeSelectionSetSuffix indentLevel spec
    in
        indent indentLevel ("..." ++ typeConditionString ++ directivesString ++ selectionSetString)


encodeVariableDefinition : VariableDefinition -> String
encodeVariableDefinition { name, variableType, defaultValue } =
    let
        defaultValueString =
            defaultValue
                |> Maybe.map (((++) " = ") << encodeArgValue)
                |> Maybe.withDefault ""
    in
        "$" ++ name ++ ": " ++ variableType ++ defaultValueString


encodeVariableDefinitionList : List VariableDefinition -> String
encodeVariableDefinitionList variableDefinitions =
    if List.isEmpty variableDefinitions then
        ""
    else
        "(" ++ String.join ", " (List.map encodeVariableDefinition variableDefinitions) ++ ")"


encodeQueryBuilder : Builder Query -> Result (List BuilderError) String
encodeQueryBuilder (Builder errs { name, variables, spec }) =
    if List.isEmpty errs then
        let
            nameAndVariables =
                Maybe.withDefault "" name ++ encodeVariableDefinitionList variables

            spacer =
                if String.isEmpty nameAndVariables then
                    ""
                else
                    " "
        in
            Ok ("query" ++ spacer ++ nameAndVariables ++ encodeSelectionSetSuffix 0 spec)
    else
        Err errs
