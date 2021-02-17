module GraphQL.Request exposing
    ( Document
    , Request
    )

import GraphQL.Request.Document.AST as AST
import GraphQL.Request.Document.AST.Serialize exposing (serializeDocument)
import Json.Decode exposing (Decoder)


type Document
    = Document
        { ast : AST.Document
        , serialized : String
        }


type Request operations result
    = Request
        { document : Document
        , operationName : Maybe String
        , variableValues : List ( String, AST.ConstantValue )
        , decoder : Decoder result
        }


document : AST.Document -> Document
document ast =
    Document
        { ast = ast
        , serialized = serializeDocument ast
        }
