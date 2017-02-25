module GraphQL.Request.AST exposing (..)


type Name
    = Name String


type Document
    = Document (List Definition)


type Definition
    = OperationDefinition OperationDefinitionInfo
    | QueryShorthand SelectionSet
    | FragmentDefinition FragmentDefinitionInfo


type alias OperationDefinitionInfo =
    { operationType : OperationType
    , name : Maybe Name
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
    { fieldAlias : Maybe Name
    , name : Name
    , arguments : List Argument
    , directives : List Directive
    , selectionSet : SelectionSet
    }


type Argument
    = Argument Name Value


type alias FragmentSpreadInfo =
    { fragmentName : FragmentName
    , directives : List Directive
    }


type alias InlineFragmentInfo =
    { typeCondition : Maybe Name
    , directives : List Directive
    , selectionSet : SelectionSet
    }


type alias FragmentDefinitionInfo =
    { fragmentName : FragmentName
    , typeCondition : Name
    , directives : List Directive
    , selectionSet : SelectionSet
    }


type FragmentName
    = FragmentName String


type Value
    = VariableValue Name
    | ScalarValue ConstantScalarValue
    | ListValue (List Value)
    | ObjectValue (List ( Name, Value ))


type ConstantValue
    = ConstantScalarValue ConstantScalarValue
    | ConstantListValue (List ConstantValue)
    | ConstantObjectValue (List ( Name, ConstantValue ))


type ConstantScalarValue
    = IntValue Int
    | FloatValue Float
    | StringValue String
    | BooleanValue Bool
    | NullValue
    | EnumValue EnumString


type EnumString
    = EnumString String


type VariableDefinition
    = VariableDefinition VariableDefinitionInfo


type alias VariableDefinitionInfo =
    { name : Name
    , variableType : Type
    , defaultValue : Maybe ConstantValue
    }


type NullableType
    = NamedType Name
    | ListType Type


type Type
    = Nullable NullableType
    | NonNull NullableType


type Directive
    = Directive DirectiveInfo


type alias DirectiveInfo =
    { name : Name
    , arguments : List Argument
    }
