module GraphQL.Schema.CodeGen
    exposing
        ( modulesFromSchema
        )

import GraphQL.Schema as Schema exposing (Schema)
import GraphQL.Schema.CodeGen.Module exposing (..)
import Dict exposing (Dict)
import Regex
import String


-- templateRegex : Regex.Regex
-- templateRegex =
--     Regex.regex "{#(>*)\\s*(\\w+)\\s*#}"
-- template : String -> Dict String String -> String
-- template body substitutions =
--     Regex.replace Regex.All templateRegex <|
--         \match ->
--             case match.submatches of
--                 [ Just indentation, Just key ] ->
--                     case Dict.get key substitutions of
--                         Nothing ->
--                             ""
--                         Just substitution ->
--                             indent (String.length indentation) substitution
-- indent : Int -> String -> String
-- indent level string =
--     if level < 1 then
--         string
--     else
--         let
--             prefix =
--                 String.repeat level "    "
--         in
--             string
--                 |> String.split "\n"
--                 |> List.map (\line -> prefix line)
--                 |> String.join "\n"
-- type Error
--     = TypeRefNotFound { referringType : String, referredType : String }
-- modulesFromSchema : Schema -> Dict String String
-- modulesFromSchema schema =
--     Dict.empty
-- resolveTypeRef : String -> String -> Dict String Schema.NamedType -> Result (List Error) Schema.NamedType
-- resolveTypeRef referringType referredType types =
--     case Dict.get referredType types of
--         Just namedType ->
--             Ok namedType
--         Nothing ->
--             Err [ TypeRefNotFound { referringType = referringType, referredType = referredType } ]
-- -- moduleFromNamedType :
-- --     Dict String Schema.NamedType
-- --     -> Schema.NamedType
-- --     -> Result (List Error) (Maybe ElmModule)
-- -- moduleFromNamedType types namedType =
-- --     case namedType of
-- --         Schema.ObjectType info ->
-- --             Result.map Just (moduleFromObjectType types info)
-- --         _ ->
-- --             Ok Nothing
-- declarationsFromScalarFieldTemplate : Dict String String ->
-- declarationsFromScalarField : String -> Schema.Field -> Result (List Error) String
-- declarationsFromScalarField decodedTypeName field =
-- declarationsFromScalarField decodedTypeName field =
--     field.name ++ " : B.SelectionSpec B.Field " ++ decodedTypeName ++ " vars\n" ++
--     field.name ++ " =\n" ++
--     "    B.field \"" ++ field.name ++ "\" []"

decodedTypeNameFromTypeRef typeRef =


specFromScalarTypeRef typeRef =
    case typeRef of
        Schema.Ref "Int" ->
            Symbol "B.int"

        Schema.Ref "Float" ->
            Symbol "B.float"

        Schema.Ref "String" ->
            Symbol "B.string"

        Schema.Ref "Bool" ->
            Symbol "B.bool"

        Schema.Ref "ID" ->
            Symbol "B.id"

        Schema.Ref "Float" ->
            Symbol "B.float"

        Schema.Ref other ->
            Symbol "B.string"
        
        Schema.List inner ->
            


bindingFromScalarField fieldName fieldTypeRef =
    { binding =
        { name = fieldName
        , args = []
        , typeAnnotation =
            Just <|
                ConstructedType "B.SelectionSpec"
                    [ TypeIdentifier "B.Field"
                    , TypeIdentifier (decodedTypeNameFromTypeRef fieldTypeRef)
                    , TypeIdentifier "vars"
                    ]
        , value =
            Application
                [ Symbol "B.field"
                , StringLiteral fieldName
                , List []
                , specFromScalarTypeRef fieldTypeRef
                ]
        }
    }



-- declarationsFromScalarField : String -> Schema.Field -> Result (List Error) (List Statement)
-- declarationsFromScalarField decodedTypeName field =
--     let
--         x =
--             1
--     in
--         Ok
--             [ FunctionTypeDeclaration field.name
--                 (TypeConstructor [ "B", "SelectionSpec" ]
--                     [ TypeConstructor [ "B", "Field" ] []
--                     , TypeConstructor [ decodedTypeName ] []
--                     , TypeVariable "vars"
--                     ]
--                 )
--             , FunctionDeclaration field.name
--                 []
--                 (Application
--                     (Application
--                         (Application
--                             (Access (Variable [ "B" ]) [ "field" ])
--                             (String field.name)
--                         )
--                         (List [])
--                     )
--                     (Access (Variable [ "B" ]) [ field.name ])
--                 )
--             ]
-- appendListResult : Result (List err) (List ok) -> Result (List err) (List ok) -> Result (List err) (List ok)
-- appendListResult r1 r2 =
--     case r1 of
--         Ok xs ->
--             case r2 of
--                 Ok ys ->
--                     Ok (xs ++ ys)
--                 _ ->
--                     r2
--         Err xs ->
--             case r2 of
--                 Err ys ->
--                     Err (xs ++ ys)
--                 _ ->
--                     r1
-- concatListResults : List (Result (List Error) (List a)) -> Result (List Error) (List a)
-- concatListResults results =
--     results
--         |> List.foldr appendListResult (Ok [])
-- moduleFromObjectType :
--     Dict String Schema.NamedType
--     -> Schema.ObjectTypeInfo
--     -> Result (List Error) ElmModule
-- moduleFromObjectType types objectType =
--     let
--         fieldDeclarations =
--             objectType.fields
--                 |> List.map declarationsFromField
--                 |> concatListResults
--     in
--         Ok
--             { filename = "GraphQLSchema/Types/" ++ objectType.name ++ ".elm"
--             , ast =
--                 [ ModuleDeclaration
--                     [ "GraphQLSchema", "Types", objectType.name ]
--                     AllExport
--                 , ImportStatement [ "GraphQL", "Request", "Builder" ] (Just "B") Nothing
--                 ]
--             }
