module Array.Extra.All exposing (recursiveGet, recursiveLast, withFold, withListAll)

import Array exposing (Array)
import Array.Extra


recursiveLast : (a -> Bool) -> Array a -> Bool
recursiveLast isOkay =
    \array ->
        case array |> Array.get ((array |> Array.length) - 1) of
            Nothing ->
                True

            Just last ->
                if isOkay last then
                    recursiveLast isOkay (Array.Extra.pop array)

                else
                    False


withListAll : (a -> Bool) -> Array a -> Bool
withListAll isOkay =
    \array ->
        array
            |> Array.toList
            |> List.all isOkay


withFold : (a -> Bool) -> Array a -> Bool
withFold isOkay =
    \array ->
        array
            |> Array.foldl
                (\element soFar -> soFar && (element |> isOkay))
                True


recursiveGet : (a -> Bool) -> Array a -> Bool
recursiveGet isOkay array =
    recursiveGetFromIndex 0 isOkay array


recursiveGetFromIndex : Int -> (a -> Bool) -> Array a -> Bool
recursiveGetFromIndex index isOkay array =
    case Array.get index array |> Maybe.map isOkay of
        Just False ->
            False

        Just True ->
            recursiveGetFromIndex (index + 1) isOkay array

        Nothing ->
            True
