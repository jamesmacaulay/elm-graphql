module GraphQL.Request.Document.AST.Util
    exposing
        ( responseKey
        )

import GraphQL.Request.Document.AST as AST


responseKey : AST.FieldInfo -> String
responseKey fieldInfo =
    case fieldInfo.alias of
        Nothing ->
            fieldInfo.name

        Just alias ->
            alias
