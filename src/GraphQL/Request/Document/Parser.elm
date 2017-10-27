module GraphQL.Request.Document.Parser exposing (parseDocument)

import GraphQL.Request.Document.AST as AST
import Combine exposing (..)
import Hex
import Char.CodePoint


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


fractionalPartRE =
    ".[0-9]+"


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
    (string "\\u" *> regex "[0-9A-Fa-f]{4}")
        |> map (Hex.fromString >> Result.map Char.CodePoint.toString)
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



-- TODO:
-- <|> fragmentSpread
-- <|> inlineFragment


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
        <*> (ignored *> directives)
        <*> (ignored *> (lazy (\_ -> selectionSet |> maybe |> map (Maybe.withDefault (AST.SelectionSet [])))))
        |> map AST.Field


document : Parser s AST.Document
document =
    (many (ignored *> definition) <* ignored)
        |> map AST.Document


definition : Parser s AST.Definition
definition =
    queryShorthand
