module GraphQL.CoreUtil.List
    exposing
        ( removeDuplicates
        )


removeDuplicates : List a -> List a
removeDuplicates xs =
    case xs of
        [] ->
            []

        first :: rest ->
            if List.any ((==) first) rest then
                removeDuplicates rest
            else
                first :: removeDuplicates rest
