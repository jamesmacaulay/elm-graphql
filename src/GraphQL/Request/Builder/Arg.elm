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
        , enum
        , object
        , list
        , getAST
        , getVariables
        )

{-| The functions in this module let you construct argument values that you can pass to fields and directives using the functions in [`GraphQL.Request.Builder`](GraphQL-Request-Builder).

@docs Value, variable, int, float, string, bool, true, false, null, enum, object, list, getAST, getVariables
-}

import GraphQL.Request.Document.AST as AST
import GraphQL.Request.Builder.Variable as Variable


{-| An argument value, which might be either a constant or a variable. The `vars` parameter is the type of Elm value that variables will extract their values from.
-}
type Value vars
    = Value AST.ArgumentValue (List (Variable.Variable vars))


{-| Construct a variable argument value.
-}
variable : Variable.Variable vars -> Value vars
variable var =
    Value (AST.VariableValue () (Variable.name var)) [ var ]


{-| Construct a constant GraphQL `Int` argument value from an Elm `Int`.
-}
int : Int -> Value vars
int x =
    Value (AST.IntValue x) []


{-| Construct a constant GraphQL `Float` argument value from an Elm `Float`.
-}
float : Float -> Value vars
float x =
    Value (AST.FloatValue x) []


{-| Construct a constant GraphQL `String` argument value from an Elm `String`.
-}
string : String -> Value vars
string x =
    Value (AST.StringValue x) []


{-| Construct a constant GraphQL `Boolean` argument value from an Elm `Bool`.
-}
bool : Bool -> Value vars
bool x =
    Value (AST.BooleanValue x) []


{-| The GraphQL `true` value.
-}
true : Value vars
true =
    bool True


{-| The GraphQL `false` value.
-}
false : Value vars
false =
    bool False


{-| The GraphQL `null` value.
-}
null : Value vars
null =
    Value AST.NullValue []


{-| Construct a GraphQL Enum value from a `String`.
-}
enum : String -> Value vars
enum symbol =
    Value (AST.EnumValue symbol) []


{-| Constructs a GraphQL Input Object value from a list of key-value pairs.
-}
object : List ( String, Value vars ) -> Value vars
object pairs =
    Value
        (AST.ObjectValue (pairs |> List.map (\( k, Value ast _ ) -> ( k, ast ))))
        (pairs |> List.concatMap (\( _, Value _ vars ) -> vars))


{-| Constructs a GraphQL List from an Elm `List` of `Value`s.
-}
list : List (Value vars) -> Value vars
list values =
    Value
        (AST.ListValue (values |> List.map (\(Value ast _) -> ast)))
        (values |> List.concatMap (\(Value _ vars) -> vars))


{-| Returns the AST (abstract syntax tree) representation of a `Value`.
-}
getAST : Value vars -> AST.ArgumentValue
getAST (Value ast _) =
    ast


{-| Returns a `List` of any `Variable`s used in the given `Value`.
-}
getVariables : Value vars -> List (Variable.Variable vars)
getVariables (Value _ vars) =
    vars
