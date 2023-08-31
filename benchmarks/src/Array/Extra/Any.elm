module Array.Extra.Any exposing (recursiveGet, recursiveLast, withFold, withList)

import Array exposing (Array)
import Array.Extra


withList : (a -> Bool) -> Array a -> Bool
withList isOkay =
    \array ->
        array
            |> Array.toList
            |> List.any isOkay


withFold : (a -> Bool) -> Array a -> Bool
withFold isOkay =
    \array ->
        array
            |> Array.foldl
                (\element soFar -> soFar || (element |> isOkay))
                False


recursiveLast : (a -> Bool) -> Array a -> Bool
recursiveLast isOkay =
    \array ->
        -- read & write is faster on the last element
        case array |> Array.get ((array |> Array.length) - 1) of
            Nothing ->
                False

            Just last ->
                if last |> isOkay then
                    True

                else
                    array |> Array.Extra.pop |> recursiveLast isOkay


recursiveGet : (a -> Bool) -> Array a -> Bool
recursiveGet isOkay array =
    recursiveGetFromIndex 0 isOkay array


recursiveGetFromIndex : Int -> (a -> Bool) -> Array a -> Bool
recursiveGetFromIndex index isOkay array =
    case Array.get index array |> Maybe.map isOkay of
        Just True ->
            True

        Just False ->
            recursiveGetFromIndex (index + 1) isOkay array

        Nothing ->
            False
