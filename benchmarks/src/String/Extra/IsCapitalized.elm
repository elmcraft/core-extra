module String.Extra.IsCapitalized exposing (leftAny, leftToList, sliceAny, sliceToList, toList, uncons)


uncons : String -> Bool
uncons str =
    case String.uncons str of
        Just ( firstChar, _ ) ->
            Char.isUpper firstChar

        Nothing ->
            False


toList : String -> Bool
toList str =
    case String.toList str of
        firstChar :: _ ->
            Char.isUpper firstChar

        [] ->
            False


leftToList : String -> Bool
leftToList str =
    case String.toList (String.left 1 str) of
        firstChar :: _ ->
            Char.isUpper firstChar

        [] ->
            False


sliceToList : String -> Bool
sliceToList str =
    case String.toList (String.slice 0 1 str) of
        firstChar :: _ ->
            Char.isUpper firstChar

        [] ->
            False


sliceAny : String -> Bool
sliceAny str =
    String.any Char.isUpper (String.slice 0 1 str)


leftAny : String -> Bool
leftAny str =
    String.any Char.isUpper (String.left 1 str)
