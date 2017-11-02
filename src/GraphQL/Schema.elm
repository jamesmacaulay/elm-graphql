module GraphQL.Schema exposing (..)

import Dict exposing (Dict)


type alias Schema =
    { queryType : String
    , mutationType : Maybe String
    , subscriptionType : Maybe String
    , types : Dict String NamedType
    , directives : List Directive
    }


type NamedType
    = ScalarType ScalarTypeInfo
    | ObjectType ObjectTypeInfo
    | UnionType UnionTypeInfo
    | InterfaceType InterfaceTypeInfo
    | EnumType EnumTypeInfo
    | InputObjectType InputObjectTypeInfo


type TypeRef
    = TypeRef Nullability CoreTypeRef


type Nullability
    = Nullable
    | NonNull


type CoreTypeRef
    = NamedTypeRef String
    | ListTypeRef TypeRef


type alias ScalarTypeInfo =
    { name : String
    , description : Maybe String
    }


type alias ObjectTypeInfo =
    { name : String
    , description : Maybe String
    , fields : List Field
    , interfaces : List TypeRef
    }


type alias UnionTypeInfo =
    { name : String
    , description : Maybe String
    , possibleTypes : List TypeRef
    }


type alias InterfaceTypeInfo =
    { name : String
    , description : Maybe String
    , fields : List Field
    , possibleTypes : List TypeRef
    }


type alias EnumTypeInfo =
    { name : String
    , description : Maybe String
    , enumValues : List EnumValue
    }


type alias InputObjectTypeInfo =
    { name : String
    , description : Maybe String
    , inputFields : List InputValue
    }


type alias Field =
    { name : String
    , description : Maybe String
    , args : List InputValue
    , valueType : TypeRef
    , isDeprecated : Bool
    , deprecationReason : Maybe String
    }


type alias InputValue =
    { name : String
    , description : Maybe String
    , valueType : TypeRef
    , defaultValue : Maybe String
    }


type alias EnumValue =
    { name : String
    , description : Maybe String
    , isDeprecated : Bool
    , deprecationReason : Maybe String
    }


type alias Directive =
    { name : String
    , description : Maybe String
    , locations : List DirectiveLocation
    , args : List InputValue
    }


type DirectiveLocation
    = QueryLocation
    | MutationLocation
    | FieldLocation
    | FragmentDefinitionLocation
    | FragmentSpreadLocation
    | InlineFragmentLocation
