module GraphQL.Request.Builder.TypeRef
    exposing
        ( TypeRef
        , namedType
        , list
        , nullable
        , int
        , float
        , string
        , boolean
        , id
        )

import GraphQL.Request.Document.AST as AST


type alias TypeRef =
    AST.TypeRef


namedType : String -> TypeRef
namedType =
    AST.TypeRef AST.NonNull << AST.NamedTypeRef


list : TypeRef -> TypeRef
list =
    AST.TypeRef AST.NonNull << AST.ListTypeRef


nullable : TypeRef -> TypeRef
nullable (AST.TypeRef _ coreTypeRef) =
    AST.TypeRef AST.Nullable coreTypeRef


int : TypeRef
int =
    namedType "Int"


float : TypeRef
float =
    namedType "Float"


string : TypeRef
string =
    namedType "String"


boolean : TypeRef
boolean =
    namedType "Boolean"


id : TypeRef
id =
    namedType "ID"
