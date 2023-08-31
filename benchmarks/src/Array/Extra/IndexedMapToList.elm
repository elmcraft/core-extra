module Array.Extra.IndexedMapToList exposing (withArrayIndexedMap, withFoldr, withListIndexedMap, withToIndexedList)

import Array exposing (Array)


withFoldr : (Int -> a -> b) -> Array a -> List b
withFoldr mapIndexAndElement =
    \array ->
        Array.foldr
            (\element ( i, soFar ) ->
                ( i - 1
                , soFar |> (::) (mapIndexAndElement i element)
                )
            )
            ( Array.length array - 1, [] )
            array
            |> Tuple.second


withToIndexedList : (Int -> b -> a) -> Array b -> List a
withToIndexedList mapIndexAndValue =
    \array ->
        array
            |> Array.toIndexedList
            |> List.map (\( i, v ) -> mapIndexAndValue i v)


withListIndexedMap : (Int -> a -> b) -> Array a -> List b
withListIndexedMap mapIndexAndValue =
    \array ->
        array
            |> Array.toList
            |> List.indexedMap mapIndexAndValue


withArrayIndexedMap : (Int -> a -> b) -> Array a -> List b
withArrayIndexedMap mapIndexAndValue =
    \array ->
        array
            |> Array.indexedMap mapIndexAndValue
            |> Array.toList
