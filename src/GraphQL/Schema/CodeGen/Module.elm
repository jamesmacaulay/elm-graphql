module GraphQL.Schema.CodeGen.Module exposing (..)


type alias Module =
    { name : String
    , description : Maybe String
    , imports : List String
    , types : List UnionTypeDeclaration
    , typeAliases : List TypeAliasDeclaration
    , bindings : List TopLevelBinding
    }


type UnionTypeExposeLevel
    = ExposedWithConstructors
    | ExposedWithoutConstructors
    | Hidden


type alias UnionTypeDeclaration =
    { name : String
    , params : List String
    , description : Maybe String
    , variants : List ( String, List TypeReference )
    , exposeLevel : UnionTypeExposeLevel
    }


type TypeAliasDeclaration
    = TypeAliasDeclaration
        { name : String
        , params : List String
        , description : Maybe String
        , typeReference : TypeReference
        , isExposed : Bool
        }


type TypeReference
    = TypeIdentifier String
    | ConstructedType String (List TypeReference)
    | TupleType (List TypeReference)
    | RecordType (List ( String, List TypeReference ))
    | FunctionType (List TypeReference)


type alias TopLevelBinding =
    { binding : Binding
    , description : Maybe String
    , isExposed : Bool
    }


type alias Binding =
    { name : String
    , args : List Pattern
    , typeAnnotation : Maybe TypeReference
    , value : Expression
    }


type alias Pattern =
    String


type Expression
    = IntLiteral Int
    | FloatLiteral Float
    | StringLiteral String
    | CharLiteral Char
    | Tuple (List Expression)
    | List (List Expression)
    | Symbol String
    | If Expression Expression Expression
    | Case Expression (List ( Pattern, Expression ))
    | Let Binding Expression
    | Application (List Expression)
    | Record (List ( String, Expression ))


int : Int -> Expression
int =
    IntLiteral


float : Float -> Expression
float =
    FloatLiteral


string : String -> Expression
string =
    StringLiteral


char : Char -> Expression
char =
    CharLiteral


tuple : List Expression -> Expression
tuple =
    Tuple


list : List Expression -> Expression
list =
    List


symbol : String -> Expression
symbol =
    Symbol


if_ : Expression -> Expression -> Expression -> Expression
if_ =
    If
