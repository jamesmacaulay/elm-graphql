module GraphQL.Request.Document.Parser exposing (parseDocument)

import GraphQL.Request.Document.AST as AST
import Combine exposing (..)
import Json.Decode


parseDocument : String -> Result (ParseErr ()) (ParseOk () AST.Document)
parseDocument =
    parse document


sourceCharacterRE : String
sourceCharacterRE =
    "[\\u0009\\u000A\\u000D\\u0020-\\uFFFF]"


unicodeBomRE : String
unicodeBomRE =
    "\\ufeff"


whitespaceRE : String
whitespaceRE =
    "[\\t ]"


lineTerminatorRE : String
lineTerminatorRE =
    "(?:\\n|\\r(?!\\n)|\\r\\n)"


commentRE : String
commentRE =
    "#(?:(?!" ++ lineTerminatorRE ++ ")" ++ sourceCharacterRE ++ ")*"


commaRE : String
commaRE =
    ","


ignoredRE : String
ignoredRE =
    "(?:"
        ++ unicodeBomRE
        ++ "|"
        ++ whitespaceRE
        ++ "|"
        ++ lineTerminatorRE
        ++ "|"
        ++ commentRE
        ++ "|"
        ++ commaRE
        ++ ")*"


ignored : Parser s ()
ignored =
    regex ignoredRE $> ()


name : Parser s String
name =
    regex "[_A-Za-z][_0-9A-Za-z]*"


integerPartRE : String
integerPartRE =
    "-?(?:0|[1-9][0-9]*)"


fractionalPartRE : String
fractionalPartRE =
    ".[0-9]+"


exponentPartRE : String
exponentPartRE =
    "[eE][+-]?[0-9]+"


floatValueRE : String
floatValueRE =
    integerPartRE
        ++ "(?:(?:"
        ++ fractionalPartRE
        ++ "(?:"
        ++ exponentPartRE
        ++ ")?)|(?:"
        ++ exponentPartRE
        ++ ")?)"


unwrapResult : Parser s (Result String a) -> Parser s a
unwrapResult =
    andThen
        (\result ->
            case result of
                Ok x ->
                    succeed x

                Err msg ->
                    fail msg
        )


intValue : Parser s (AST.Value a)
intValue =
    regex integerPartRE
        |> map (String.toInt >> Result.map AST.IntValue)
        |> unwrapResult


floatValue : Parser s (AST.Value a)
floatValue =
    regex floatValueRE
        |> map (String.toFloat >> Result.map AST.FloatValue)
        |> unwrapResult


escapedUnicode : Parser s String
escapedUnicode =
    (regex "\\\\u[0-9A-Fa-f]{4}")
        |> map (\code -> Json.Decode.decodeString Json.Decode.string ("\"" ++ code ++ "\""))
        |> unwrapResult


escapedDoubleQuote : Parser s String
escapedDoubleQuote =
    string "\\\"" $> "\""


escapedBackSlash : Parser s String
escapedBackSlash =
    string "\\\\" $> "\\"


escapedForwardSlash : Parser s String
escapedForwardSlash =
    string "\\/" $> "/"


escapedBackspace : Parser s String
escapedBackspace =
    string "\\b" $> "\x08"


escapedFormFeed : Parser s String
escapedFormFeed =
    string "\\f" $> "\x0C"


escapedNewline : Parser s String
escapedNewline =
    string "\\n" $> "\n"


escapedCarriageReturn : Parser s String
escapedCarriageReturn =
    string "\\r" $> "\x0D"


escapedTab : Parser s String
escapedTab =
    string "\\t" $> "\t"


stringCharacter : Parser s String
stringCharacter =
    (lookAhead (regex ("[^" ++ "\"\\\\\\r\\n]")) *> regex sourceCharacterRE)
        <|> escapedUnicode
        <|> escapedDoubleQuote
        <|> escapedBackSlash
        <|> escapedForwardSlash
        <|> escapedBackspace
        <|> escapedFormFeed
        <|> escapedNewline
        <|> escapedCarriageReturn
        <|> escapedTab


stringValue : Parser s (AST.Value a)
stringValue =
    (string "\"" *> many stringCharacter <* string "\"")
        |> map (String.join "" >> AST.StringValue)


variableValue : Parser s (AST.Value ())
variableValue =
    (string "$" *> name)
        |> map (AST.VariableValue ())


nonVariableScalarValue : Parser s (AST.Value a)
nonVariableScalarValue =
    intValue
        <|> floatValue
        <|> stringValue
        <|> (regex "\\btrue\\b" $> AST.BooleanValue True)
        <|> (regex "\\bfalse\\b" $> AST.BooleanValue False)
        <|> (regex "\\bnull\\b" $> AST.NullValue)
        <|> (map AST.EnumValue name)


value : (Parser s (AST.Value a) -> Parser s (AST.Value b)) -> Parser s (AST.Value b)
value transform =
    transform nonVariableScalarValue
        <|> lazy (\_ -> listValue transform)
        <|> lazy (\_ -> objectValue transform)


argumentValue : Parser s AST.ArgumentValue
argumentValue =
    value (or variableValue)


constantValue : Parser s AST.ConstantValue
constantValue =
    value identity


listValue : (Parser s (AST.Value a) -> Parser s (AST.Value b)) -> Parser s (AST.Value b)
listValue transform =
    (string "[" *> many (ignored *> value transform) <* ignored <* string "]")
        |> map AST.ListValue


objectField : (Parser s (AST.Value a) -> Parser s (AST.Value b)) -> Parser s ( String, AST.Value b )
objectField transform =
    (,) <$> (name <* ignored <* string ":") <*> (ignored *> value transform)


objectValue : (Parser s (AST.Value a) -> Parser s (AST.Value b)) -> Parser s (AST.Value b)
objectValue transform =
    (string "{" *> many (ignored *> objectField transform) <* ignored <* string "}")
        |> map AST.ObjectValue


queryShorthand : Parser s AST.Definition
queryShorthand =
    map AST.QueryShorthand selectionSet


selectionSet : Parser s AST.SelectionSet
selectionSet =
    string "{"
        *> many (ignored *> lazy (\_ -> selection))
        <* ignored
        <* string "}"
        |> map AST.SelectionSet


selection : Parser s AST.Selection
selection =
    field
        <|> fragmentSpread
        <|> lazy (\_ -> inlineFragment)


alias : Parser s String
alias =
    name <* ignored <* string ":"


argument : Parser s ( String, AST.ArgumentValue )
argument =
    (,) <$> (name <* ignored <* string ":") <*> (ignored *> argumentValue)


arguments : Parser s (List ( String, AST.ArgumentValue ))
arguments =
    string "(" *> many (ignored *> argument) <* ignored <* string ")"


directive : Parser s AST.Directive
directive =
    AST.DirectiveInfo
        <$> (string "@" *> ignored *> name)
        <*> (ignored *> (maybe arguments |> map (Maybe.withDefault [])))
        |> map AST.Directive


directives : Parser s (List AST.Directive)
directives =
    many (ignored *> directive)


field : Parser s AST.Selection
field =
    AST.FieldInfo
        <$> (maybe alias)
        <*> (ignored *> name)
        <*> (ignored *> (maybe arguments |> map (Maybe.withDefault [])))
        <*> directives
        <*> (ignored *> (lazy (\_ -> maybe selectionSet |> map (Maybe.withDefault (AST.SelectionSet [])))))
        |> map AST.Field


fragmentName : Parser s String
fragmentName =
    name
        |> andThen
            (\name ->
                if name == "on" then
                    fail "invalid fragment name \"on\""
                else
                    succeed name
            )


fragmentSpread : Parser s AST.Selection
fragmentSpread =
    AST.FragmentSpreadInfo
        <$> (string "..." *> ignored *> fragmentName)
        <*> directives
        |> map AST.FragmentSpread


typeCondition : Parser s AST.TypeCondition
typeCondition =
    (string "on" *> ignored *> name)
        |> map AST.TypeCondition


inlineFragment : Parser s AST.Selection
inlineFragment =
    AST.InlineFragmentInfo
        <$> (string "..." *> ignored *> (maybe typeCondition))
        <*> directives
        <*> (ignored *> selectionSet)
        |> map AST.InlineFragment


document : Parser s AST.Document
document =
    (many1 (ignored *> definition) <* ignored <* end)
        |> map AST.Document


definition : Parser s AST.Definition
definition =
    operationDefinition
        <|> queryShorthand
        <|> fragmentDefinition


variable : Parser s String
variable =
    string "$" *> ignored *> name


namedType : Parser s AST.CoreTypeRef
namedType =
    map AST.NamedTypeRef name


listType : Parser s AST.CoreTypeRef
listType =
    (string "[" *> ignored *> lazy (\_ -> typeRef) <* ignored <* string "]")
        |> map AST.ListTypeRef


coreTypeRef : Parser s AST.CoreTypeRef
coreTypeRef =
    namedType <|> lazy (\_ -> listType)


nullability : Parser s AST.Nullability
nullability =
    (string "!" $> AST.NonNull) <|> (string "" $> AST.Nullable)


typeRef : Parser s AST.TypeRef
typeRef =
    (flip AST.TypeRef)
        <$> lazy (\_ -> coreTypeRef)
        <*> (ignored *> nullability)


variableDefinition : Parser s AST.VariableDefinition
variableDefinition =
    AST.VariableDefinitionInfo
        <$> (variable <* ignored <* string ":")
        <*> (ignored *> typeRef)
        <*> (ignored *> maybe constantValue)
        |> map AST.VariableDefinition


variableDefinitions : Parser s (List AST.VariableDefinition)
variableDefinitions =
    string "(" *> many (ignored *> variableDefinition) <* ignored <* string ")"


operationType : Parser s AST.OperationType
operationType =
    (string "query" $> AST.Query)
        <|> (string "mutation" $> AST.Mutation)


operationDefinition : Parser s AST.Definition
operationDefinition =
    AST.OperationDefinitionInfo
        <$> operationType
        <*> (ignored *> (maybe name))
        <*> (ignored *> (maybe variableDefinitions |> map (Maybe.withDefault [])))
        <*> directives
        <*> (ignored *> selectionSet)
        |> map AST.OperationDefinition


fragmentDefinition : Parser s AST.Definition
fragmentDefinition =
    AST.FragmentDefinitionInfo
        <$> (string "fragment" *> ignored *> fragmentName)
        <*> (ignored *> typeCondition)
        <*> directives
        <*> (ignored *> selectionSet)
        |> map AST.FragmentDefinition
