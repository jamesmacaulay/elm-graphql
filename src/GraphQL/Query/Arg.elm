module GraphQL.Query.Arg exposing (..)


type Value
    = VariableValue String
    | IntValue Int
    | FloatValue Float
    | StringValue String
    | BooleanValue Bool
    | NullValue
    | EnumValue String
    | ListValue (List Value)
    | ObjectValue (List ( String, Value ))


variable : String -> Value
variable =
    VariableValue


int : Int -> Value
int =
    IntValue


float : Float -> Value
float =
    FloatValue


string : String -> Value
string =
    StringValue


bool : Bool -> Value
bool =
    BooleanValue


null : Value
null =
    NullValue


enum : String -> Value
enum =
    StringValue


object : List ( String, Value ) -> Value
object =
    ObjectValue


list : List Value -> Value
list =
    ListValue
