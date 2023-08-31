module Array.Extra.FilterMap exposing (withCons, withListFilterMap, withPush)

import Array exposing (Array)


withPush : (a -> Maybe b) -> Array a -> Array b
withPush elementParse =
    \array ->
        Array.foldl
            (\element soFar ->
                soFar |> pushTry (element |> elementParse)
            )
            Array.empty
            array


pushTry : Maybe a -> Array a -> Array a
pushTry maybe =
    case maybe of
        Just value ->
            \array -> array |> Array.push value

        Nothing ->
            identity


withListFilterMap : (a -> Maybe mapped) -> Array a -> Array mapped
withListFilterMap tryMap =
    \array ->
        array
            |> Array.toList
            |> List.filterMap tryMap
            |> Array.fromList


withCons : (a -> Maybe mapped) -> Array a -> Array mapped
withCons tryChange =
    \array ->
        array
            |> Array.foldr
                (\el soFar -> soFar |> consTry (el |> tryChange))
                []
            |> Array.fromList


consTry : Maybe a -> List a -> List a
consTry maybeNewHead =
    \list ->
        case maybeNewHead of
            Just newHead ->
                newHead :: list

            Nothing ->
                list
