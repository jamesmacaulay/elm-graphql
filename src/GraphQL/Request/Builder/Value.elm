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
        )

import GraphQL.Request.Document.AST as AST


type alias Value a =
    AST.Value a


type alias Constant =
    AST.ConstantValue


type alias Argument =
    AST.ArgumentValue


variable : String -> Argument
variable =
    AST.VariableValue ()


int : Int -> Value a
int =
    AST.IntValue


float : Float -> Value a
float =
    AST.FloatValue


string : String -> Value a
string =
    AST.StringValue


bool : Bool -> Value a
bool =
    AST.BooleanValue


true : Value a
true =
    AST.BooleanValue True


false : Value a
false =
    AST.BooleanValue False


null : Value a
null =
    AST.NullValue


object : List ( String, Value a ) -> Value a
object =
    AST.ObjectValue


list : List (Value a) -> Value a
list =
    AST.ListValue
