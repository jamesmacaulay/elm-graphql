module GraphQL.Request.Document.AST exposing (..)


type Document
    = Document (List Definition)


type Definition
    = OperationDefinition OperationDefinitionInfo
    | QueryShorthand SelectionSet
    | FragmentDefinition FragmentDefinitionInfo


type alias OperationDefinitionInfo =
    { operationType : OperationType
    , name : Maybe String
    , variableDefinitions : List VariableDefinition
    , directives : List Directive
    , selectionSet : SelectionSet
    }


type OperationType
    = Query
    | Mutation


type SelectionSet
    = SelectionSet (List Selection)


type Selection
    = Field FieldInfo
    | FragmentSpread FragmentSpreadInfo
    | InlineFragment InlineFragmentInfo


type alias FieldInfo =
    { alias : Maybe String
    , name : String
    , arguments : List ( String, ArgumentValue )
    , directives : List Directive
    , selectionSet : SelectionSet
    }


type alias FragmentSpreadInfo =
    { name : String
    , directives : List Directive
    }


type TypeCondition
    = TypeCondition String


type alias InlineFragmentInfo =
    { typeCondition : Maybe TypeCondition
    , directives : List Directive
    , selectionSet : SelectionSet
    }


type alias FragmentDefinitionInfo =
    { name : String
    , typeCondition : TypeCondition
    , directives : List Directive
    , selectionSet : SelectionSet
    }


type Value variableConstraint
    = VariableValue variableConstraint String
    | IntValue Int
    | FloatValue Float
    | StringValue String
    | BooleanValue Bool
    | NullValue
    | EnumValue String
    | ListValue (List (Value variableConstraint))
    | ObjectValue (List ( String, Value variableConstraint ))


type alias ConstantValue =
    Value Never


type alias ArgumentValue =
    Value ()


type VariableDefinition
    = VariableDefinition VariableDefinitionInfo


type alias VariableDefinitionInfo =
    { name : String
    , variableType : TypeRef
    , defaultValue : Maybe ConstantValue
    }


type TypeRef
    = TypeRef Nullability CoreTypeRef


type Nullability
    = Nullable
    | NonNull


type CoreTypeRef
    = NamedTypeRef String
    | ListTypeRef TypeRef


type Directive
    = Directive DirectiveInfo


type alias DirectiveInfo =
    { name : String
    , arguments : List ( String, ArgumentValue )
    }
