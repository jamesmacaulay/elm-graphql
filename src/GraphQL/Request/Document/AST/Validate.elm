module GraphQL.Request.Document.AST.Validate exposing (..)

import GraphQL.Request.Document.AST as AST
import GraphQL.Request.Document.AST.Util as Util
import GraphQL.Schema as Schema exposing (Schema)
import Set exposing (Set)
import Dict exposing (Dict)


type Error
    = OperationNameUniquenessError (List String)
    | LoneAnonymousOperationError


errorMessage : Error -> String
errorMessage error =
    case error of
        OperationNameUniquenessError duplicates ->
            "Operation names must be unique, but multiple operations in this document have the same name: " ++ String.join ", " duplicates ++ "."

        LoneAnonymousOperationError ->
            "This document has multiple operations and not all of them have names. The anonymous query shorthand is only allowed when there are no other queries or mutations defined in the document."


validateOperationNameUniqueness : AST.Document -> Maybe Error
validateOperationNameUniqueness document =
    let
        duplicates =
            duplicateOperationNames document
    in
        if List.isEmpty duplicates then
            Nothing
        else
            Just (OperationNameUniquenessError duplicates)


validateLoneAnonymousOperation : AST.Document -> Maybe Error
validateLoneAnonymousOperation ((AST.Document definitions) as document) =
    if List.length definitions > 1 && isAnonymousOperationPresent document then
        Just LoneAnonymousOperationError
    else
        Nothing


validateFieldSelectionsOnObjects : Schema -> AST.Document -> Maybe Error
validateFieldSelectionsOnObjects schema document =
    Nothing


validateFieldSelectionMerging : Schema -> AST.Document -> Maybe Error
validateFieldSelectionMerging schema document =
    Nothing


validateLeafFieldSelections : Schema -> AST.Document -> Maybe Error
validateLeafFieldSelections schema document =
    Nothing


fieldsInSetCanMerge :
    Schema
    -> Dict String AST.FragmentDefinitionInfo
    -> String
    -> AST.SelectionSet
    -> Bool
fieldsInSetCanMerge schema fragments selectionSetTypeName ((AST.SelectionSet selections) as selectionSet) =
    let
        repeatedReponseKeyFields =
            fieldsInSet fragments selectionSet
                |> groupBy Util.responseKey
                |> Dict.filter (\responseKey fields -> List.length fields > 1)
    in
        True


sameResponseShape : Schema -> Dict String AST.FragmentDefinitionInfo -> String -> AST.FieldInfo -> AST.FieldInfo -> Bool
sameResponseShape schema fragments selectionSetTypeName fieldA fieldB =
    let
        typeA =
            fieldReturnType schema selectionSetTypeName fieldA

        typeB =
            fieldReturnType schema selectionSetTypeName fieldB

        go : Schema.TypeRef -> Schema.TypeRef -> Bool
        go a b =
            case ( a, b ) of
                ( Schema.TypeRef nullabilityA coreA, Schema.TypeRef nullabilityB coreB ) ->
                    if nullabilityA /= nullabilityB then
                        False
                    else
                        case ( coreA, coreB ) of
                            ( Schema.NamedTypeRef innerTypeNameA, Schema.NamedTypeRef innerTypeNameB ) ->
                                if innerTypeNameA == innerTypeNameB then
                                    True
                                else
                                    let
                                        innerTypeA =
                                            Dict.get innerTypeNameA schema.types

                                        innerTypeB =
                                            Dict.get innerTypeNameB schema.types
                                    in
                                        if (Maybe.map isCompositeType innerTypeA == Just True && Maybe.map isCompositeType innerTypeB == Just True) then
                                            let
                                                mergedSet =
                                                    mergeSelectionSets fieldA.selectionSet fieldB.selectionSet
                                            in
                                                True
                                        else
                                            False

                            ( Schema.ListTypeRef innerA, Schema.ListTypeRef innerB ) ->
                                go innerA innerB

                            _ ->
                                False
    in
        Maybe.map2 go typeA typeB
            |> Maybe.withDefault True


mergeSelectionSets : AST.SelectionSet -> AST.SelectionSet -> AST.SelectionSet
mergeSelectionSets (AST.SelectionSet selectionsA) (AST.SelectionSet selectionsB) =
    AST.SelectionSet (selectionsA ++ selectionsB)


isCompositeType : Schema.NamedType -> Bool
isCompositeType namedType =
    case namedType of
        Schema.ScalarType _ ->
            False

        Schema.EnumType _ ->
            False

        _ ->
            True



-- typesCanMerge : Schema.TypeRef -> Schema.TypeRef -> Bool
-- typesCanMerge typeA typeB =
--     case (typeA, typeB) of
--         (Schema.ListTypeRef innerA, Schema.ListTypeRef innerB) ->


fieldsDefinedOnObjectOrInterface : Schema.NamedType -> List Schema.Field
fieldsDefinedOnObjectOrInterface namedType =
    case namedType of
        Schema.ObjectType { fields } ->
            fields

        Schema.InterfaceType { fields } ->
            fields

        _ ->
            []


fieldReturnType : Schema -> String -> AST.FieldInfo -> Maybe Schema.TypeRef
fieldReturnType schema selectionSetTypeName field =
    if field.name == "__typename" then
        Just <|
            Schema.TypeRef Schema.NonNull (Schema.NamedTypeRef "String")
    else
        schema.types
            |> Dict.get selectionSetTypeName
            |> Maybe.andThen
                (fieldsDefinedOnObjectOrInterface >> List.filter (.name >> (==) field.name) >> List.head)
            |> Maybe.map .valueType


fieldsInSet : Dict String AST.FragmentDefinitionInfo -> AST.SelectionSet -> List AST.FieldInfo
fieldsInSet fragments (AST.SelectionSet selections) =
    selections
        |> List.concatMap
            (\selection ->
                case selection of
                    AST.Field info ->
                        [ info ]

                    AST.FragmentSpread { name } ->
                        case Dict.get name fragments of
                            Nothing ->
                                []

                            Just fragment ->
                                fieldsInSet (Dict.remove name fragments) fragment.selectionSet

                    AST.InlineFragment { selectionSet } ->
                        fieldsInSet fragments selectionSet
            )


duplicateOperationNames : AST.Document -> List String
duplicateOperationNames (AST.Document definitions) =
    definitions
        |> List.filterMap operationName
        |> duplicates


isAnonymousOperationPresent : AST.Document -> Bool
isAnonymousOperationPresent (AST.Document definitions) =
    definitions
        |> List.filter isAnonymousOperation
        |> List.isEmpty
        |> not



--


duplicates : List comparable -> List comparable
duplicates xs =
    let
        go x { all, duplicates } =
            { all = Set.insert x all
            , duplicates =
                if Set.member x all then
                    Set.insert x duplicates
                else
                    duplicates
            }
    in
        xs
            |> List.foldl go { all = Set.empty, duplicates = Set.empty }
            |> .duplicates
            |> Set.toList


groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy f =
    List.foldr
        (\x d ->
            Dict.update
                (f x)
                (Maybe.map (\xs -> x :: xs) >> Maybe.withDefault [ x ] >> Just)
                d
        )
        Dict.empty


operationName : AST.Definition -> Maybe String
operationName definition =
    case definition of
        AST.OperationDefinition { name } ->
            name

        AST.QueryShorthand _ ->
            Nothing

        AST.FragmentDefinition _ ->
            Nothing


isAnonymousOperation : AST.Definition -> Bool
isAnonymousOperation definition =
    case operationName definition of
        Nothing ->
            True

        Just _ ->
            False
