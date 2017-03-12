module GraphQL.Request.Builder.Value
    exposing
        ( Value
        , Constant
        , Argument
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


type Value variableConstraint variableSource
    = Value (AST.Value variableConstraint) (List (Variable variableSource))


type alias Constant =
    Value Never Never


type alias Argument variableSource =
    Value () variableSource


variable : Variable source -> Argument source
variable var =
    Value (AST.VariableValue () (Variable.name var)) [ var ]


int : Int -> Value a b
int x =
    Value (AST.IntValue x) []


float : Float -> Value a b
float x =
    Value (AST.FloatValue x) []


string : String -> Value a b
string x =
    Value (AST.StringValue x) []


bool : Bool -> Value a b
bool x =
    Value (AST.BooleanValue x) []


true : Value a b
true =
    bool True


false : Value a b
false =
    bool False


null : Value a b
null =
    Value AST.NullValue []


object : List ( String, Value a b ) -> Value a b
object pairs =
    Value
        (AST.ObjectValue (pairs |> List.map (\( k, Value ast _ ) -> ( k, ast ))))
        (pairs |> List.concatMap (\( _, Value _ vars ) -> vars))


list : List (Value a b) -> Value a b
list values =
    Value
        (AST.ListValue (values |> List.map (\(Value ast _) -> ast)))
        (values |> List.concatMap (\(Value _ vars) -> vars))


getAST : Value a b -> AST.Value a
getAST (Value ast _) =
    ast


getVariables : Value a b -> List (Variable b)
getVariables (Value _ vars) =
    vars
