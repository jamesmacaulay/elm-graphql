module GraphQL.Request.Builder.Arg
    exposing
        ( Value
        , variable
        , int
        , float
        , string
        , bool
        , true
        , false
        , null
        , object
        , list
        , getAST
        , getVariables
        )

import GraphQL.Request.Document.AST as AST
import GraphQL.Request.Builder.Variable as Variable exposing (Variable)


type Value variableSource
    = Value AST.ArgumentValue (List (Variable variableSource))


variable : Variable variableSource -> Value variableSource
variable var =
    Value (AST.VariableValue () (Variable.name var)) [ var ]


int : Int -> Value variableSource
int x =
    Value (AST.IntValue x) []


float : Float -> Value variableSource
float x =
    Value (AST.FloatValue x) []


string : String -> Value variableSource
string x =
    Value (AST.StringValue x) []


bool : Bool -> Value variableSource
bool x =
    Value (AST.BooleanValue x) []


true : Value variableSource
true =
    bool True


false : Value variableSource
false =
    bool False


null : Value variableSource
null =
    Value AST.NullValue []


object : List ( String, Value variableSource ) -> Value variableSource
object pairs =
    Value
        (AST.ObjectValue (pairs |> List.map (\( k, Value ast _ ) -> ( k, ast ))))
        (pairs |> List.concatMap (\( _, Value _ vars ) -> vars))


list : List (Value variableSource) -> Value variableSource
list values =
    Value
        (AST.ListValue (values |> List.map (\(Value ast _) -> ast)))
        (values |> List.concatMap (\(Value _ vars) -> vars))


getAST : Value variableSource -> AST.ArgumentValue
getAST (Value ast _) =
    ast


getVariables : Value variableSource -> List (Variable variableSource)
getVariables (Value _ vars) =
    vars
